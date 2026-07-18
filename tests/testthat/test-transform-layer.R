# DuckDB 2-D data application (st_affine / transformLayer): apply a coordinate
# transform to a lazy point (x/y) layer via arithmetic and to a geometry layer via
# ST_Affine, against a live DuckDB. Geometry cases skip when the spatial extension
# is unavailable, like the other spatial tests.
# library(testthat); library(DuckDBSpatial); source("test-transform-layer.R")

.pts_layer <- function(df) {
    p <- tempfile(fileext = ".parquet")
    writeSpatialPointsParquet(df, p, spatial_sort = FALSE)
    DuckDBDataFrame::DuckDBDataFrame(p)
}

test_that("transformLayer applies a rotation to an x/y point layer", {
    pts <- .pts_layer(data.frame(x = c(1, 2, 3), y = c(0, 0, 0),
                                 gene = c("A", "B", "C")))
    # rotate 90 about origin: (x,y)->(-y,x)
    out <- as.data.frame(transformLayer(pts, ctRotation(rbind(c(0, -1), c(1, 0)))))
    out <- out[order(out$gene), ]
    expect_equal(nrow(out), 3L)
    expect_equal(out$x, c(0, 0, 0))
    expect_equal(out$y, c(1, 2, 3))
})

test_that("transformLayer applies a scale+translation sequence on x/y", {
    pts <- .pts_layer(data.frame(x = c(1, 2, 3), y = c(0, 0, 0),
                                 gene = c("A", "B", "C")))
    seqt <- ctSequence(list(ctScale(c(2, 3)), ctTranslation(c(10, 20))))
    out <- as.data.frame(transformLayer(pts, seqt))
    out <- out[order(out$gene), ]
    expect_equal(out$x, c(12, 14, 16))  # 2*x + 10
    expect_equal(out$y, c(20, 20, 20))  # 3*0 + 20
})

test_that("transformLayer accepts a bare affine matrix", {
    pts <- .pts_layer(data.frame(x = c(1, 2), y = c(3, 4)))
    # 2x3 [linear | offset]: x' = x + 1, y' = y + 2
    out <- as.data.frame(transformLayer(pts, rbind(c(1, 0, 1), c(0, 1, 2))))
    expect_setequal(out$x, c(2, 3))
    expect_setequal(out$y, c(5, 6))
})

test_that("transformLayer errors on a non-2-D transform and on missing columns", {
    pts <- .pts_layer(data.frame(x = 1, y = 2))
    up <- ctAffine(rbind(c(2, 0, 0), c(0, 3, 0), c(1, 1, 0)))  # 2D -> 3D
    expect_error(transformLayer(pts, up), "2-D")
    skip_if_not_installed("arrow")
    bp <- tempfile(fileext = ".parquet")
    arrow::write_parquet(data.frame(a = 1, b = 2), bp)
    bare <- DuckDBDataFrame::DuckDBDataFrame(bp)
    expect_error(transformLayer(bare, ctScale(c(2, 2))),
                 "no coordinate columns")
})

test_that("st_affine transforms a geometry layer via ST_Affine", {
    ok <- tryCatch({
        DuckDBDataFrame::loadExtension(DuckDBDataFrame::acquireDuckDBConn(), "spatial")
        TRUE
    }, error = function(e) FALSE)
    skip_if_not(ok, "spatial extension unavailable")
    skip_if_not_installed("sf")

    sfobj <- sf::st_as_sf(data.frame(id = c("A", "B", "C"),
                                     x = c(1, 2, 3), y = c(0, 0, 0)),
                          coords = c("x", "y"))
    p <- tempfile(fileext = ".parquet")
    writeGeoParquet(sfobj, p)
    lay <- DuckDBDataFrame::DuckDBDataFrame(p)

    out <- transformLayer(lay, ctRotation(rbind(c(0, -1), c(1, 0))), x_col = NULL)
    df <- as.data.frame(out)
    g <- sf::st_as_sfc(structure(df$geometry, class = "WKB"), EWKB = TRUE)
    xy <- unname(sf::st_coordinates(g)[, c("X", "Y")])
    expect_equal(xy[, 1], c(0, 0, 0))  # x' = -y = 0
    expect_equal(xy[, 2], c(1, 2, 3))  # y' = x
    unlink(p)
})
