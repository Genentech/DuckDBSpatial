# Tests the coord-indexed points index + writer (points-coord-index.R): the Morton
# (Z-order) sort and the coord-indexed Parquet write. The prunable query that exploits
# the layout, layerBboxRange(), is a layer engine tested in test-DuckDBColumn.R.
# library(testthat); library(DuckDBSpatial); source("setup.R"); source("test-points-coord-index.R")

.make_points <- function(n = 3000L, genes = 50L, seed = 1L) {
    set.seed(seed)
    data.frame(
        x = runif(n, 0, 100),
        y = runif(n, 0, 100),
        gene = sample(paste0("G", seq_len(genes) - 1L), n, replace = TRUE),
        stringsAsFactors = FALSE
    )
}

test_that("spatialSortPoints is a row permutation that preserves the data", {
    df <- .make_points()
    srt <- spatialSortPoints(df)
    expect_equal(nrow(srt), nrow(df))
    expect_setequal(srt$x, df$x)
    expect_equal(sort(table(srt$gene)), sort(table(df$gene)))
    # a true reordering (not the same order) for a random cloud
    expect_false(identical(srt$x, df$x))
})

test_that("spatialSortPoints is a no-op without coordinate columns", {
    df <- data.frame(a = 1:3, b = 4:6)
    expect_identical(spatialSortPoints(df), df)
    expect_identical(spatialSortPoints(df[0, ]), df[0, ])
})

test_that("spatialSortPoints clusters spatial neighbours (shared Morton generator)", {
    # two nearby points and one far one: the neighbours end up adjacent in the
    # sorted order, the far point does not. Delegates to DuckDBDataFrame::zorder().
    df <- data.frame(x = c(1, 1.1, 99), y = c(1, 1.1, 99))
    srt <- spatialSortPoints(df)
    pos <- vapply(c(1, 1.1, 99), function(v) which(srt$x == v), integer(1L))
    expect_lt(abs(pos[1] - pos[2]), abs(pos[1] - pos[3]))
    # degenerate axis + non-finite must not error
    expect_length(spatialSortPoints(data.frame(x = c(5, 5, NA), y = c(1, 2, 3)))$x, 3L)
})

test_that("writeSpatialPointsParquet round-trips (sorted and baseline)", {
    df <- .make_points()
    for (sort in c(TRUE, FALSE)) {
        p <- tempfile(fileext = ".parquet")
        on.exit(unlink(p), add = TRUE)
        writeSpatialPointsParquet(df, p, spatial_sort = sort, row_group_size = 500L)
        rt <- as.data.frame(DuckDBDataFrame(p))
        expect_equal(nrow(rt), nrow(df))
        # value-set identical regardless of on-disk order
        expect_setequal(rt$x, df$x)
        expect_setequal(rt$gene, df$gene)
    }
})

test_that("writeSpatialPointsParquet errors without coordinates", {
    expect_error(
        writeSpatialPointsParquet(data.frame(a = 1:3), tempfile(fileext = ".parquet")),
        "coordinate columns"
    )
})
