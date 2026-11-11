# Tests that sf-compatible methods on a full (multi-column) DuckDBTable/DuckDBDataFrame
# correctly scope to the one GEOMETRY column: "Group A" methods (geometry transforms, binary set
# operations) must retain every other column and preserve row count/order, matching plain sf's own
# st_set_geometry()-based convention; "Group B" methods (accessors, measures, predicates) must
# continue to return a bare, narrowed (geometry-only) result, not retain other columns.
# library(testthat); library(DuckDBSpatial); source("setup.R"); source("test-DuckDBTable-spatial.R")

# Materializes a DuckDBTable/DuckDBDataFrame's "geometry" column to a plain character (WKT)
# vector, for comparing against plain sf's own result.
.geom_wkt <- function(x) as.vector(st_as_text(x[["geometry"]]))
.geom_sfc <- function(x) st_as_sfc(.geom_wkt(x))

test_that("Group A methods retain non-geometry columns on a DuckDBTable", {
    df <- DuckDBDataFrame(spatial_path)
    df <- df[which(!is.na(spatial_wkt)), ]
    sfc <- st_as_sfc(spatial_wkt[!is.na(spatial_wkt)])
    ne <- !st_is_empty(sfc)  # exclude EMPTY geometries: sf vs DuckDB disagree on XY vs XYZ for
                             # these (semantically meaningless for an EMPTY geometry), unrelated
                             # to the column-retention fix under test here.

    non_geom_cols <- setdiff(colnames(df), "geometry")
    before <- as.data.frame(df[, non_geom_cols, drop = FALSE])

    check_retains <- function(out) {
        expect_s4_class(out, class(df))
        expect_identical(nrow(out), nrow(df))
        expect_identical(colnames(out), colnames(df))
        expect_identical(as.data.frame(out[, non_geom_cols, drop = FALSE]), before)
    }

    out <- st_centroid(df)
    check_retains(out)
    # st_centroid is exact (no curve discretization), so this matches plain sf point-for-point.
    expect_equal(.geom_sfc(out), st_centroid(sfc))

    out <- st_simplify(df, preserveTopology = FALSE)
    check_retains(out)
    expect_equal(.geom_sfc(out)[ne], st_simplify(sfc, preserveTopology = FALSE)[ne])

    out <- st_buffer(df, 1)
    check_retains(out)
    # ST_Buffer's circular-arc discretization differs between DuckDB and GEOS (different point
    # counts per arc), so an exact coordinate match is not meaningful; compare area instead
    # (robust to discretization, sensitive to a genuinely wrong buffer), and confirm it actually
    # changed from the original (not a silent no-op).
    expect_equal(as.numeric(st_area(.geom_sfc(out)[ne])),
                as.numeric(st_area(st_buffer(sfc, 1)[ne])), tolerance = 0.01)
    expect_false(isTRUE(all.equal(.geom_wkt(out), .geom_wkt(df))))
})

test_that("st_inscribed_circle retains non-geometry columns (its own endoapply-based code path)", {
    df <- DuckDBDataFrame(spatial_path)
    df <- df[which(!is.na(spatial_wkt)), ]

    non_geom_cols <- setdiff(colnames(df), "geometry")
    before <- as.data.frame(df[, non_geom_cols, drop = FALSE])

    out <- st_inscribed_circle(df, 0.01)
    expect_s4_class(out, class(df))
    expect_identical(nrow(out), nrow(df))
    expect_identical(colnames(out), colnames(df))
    expect_identical(as.data.frame(out[, non_geom_cols, drop = FALSE]), before)
})

test_that("binary set operations retain non-geometry columns on a DuckDBTable", {
    df <- DuckDBDataFrame(spatial_path)
    df <- df[which(!is.na(spatial_wkt)), ]
    sfc <- st_as_sfc(spatial_wkt[!is.na(spatial_wkt)])
    query <- st_sfc(st_point(c(30, 10)))

    non_geom_cols <- setdiff(colnames(df), "geometry")
    before <- as.data.frame(df[, non_geom_cols, drop = FALSE])

    # Our DuckDBTable methods are a per-row scalar SQL call (ST_Difference(x_geom_row_i,
    # .geom_to_sql(y))), never a join -- unlike plain sf's own default (by_element = FALSE),
    # which does a cartesian/index-paired overlay that can drop or add rows, so the ground truth
    # to compare against is one row's own element-wise result, not sf's own
    # st_difference(sfc, query) directly (whose row count need not match ours). Checking a
    # single, simple, non-empty POLYGON row (row 16, a plain POLYGON, picked to sidestep
    # GEOMETRYCOLLECTION/EMPTY-geometry corner cases in sf's own comparison machinery that are
    # unrelated to the column-retention fix under test here) is enough to confirm the geometry
    # is actually transformed correctly; column retention itself is checked across every row.
    i <- 16L
    check_set_op <- function(out, fun) {
        expect_s4_class(out, class(df))
        expect_identical(nrow(out), nrow(df))
        expect_identical(colnames(out), colnames(df))
        expect_identical(as.data.frame(out[, non_geom_cols, drop = FALSE]), before)
        expect_equal(unname(.geom_wkt(out)[i]), unname(st_as_text(fun(sfc[i], query))))
    }

    check_set_op(st_difference(df, query), st_difference)
    check_set_op(st_intersection(df, query), st_intersection)
    check_set_op(st_union(df, query), st_union)
})

test_that("Group B methods remain unaffected by the Group A fix", {
    df <- DuckDBDataFrame(spatial_path)
    df <- df[which(!is.na(spatial_wkt)), ]
    query <- st_sfc(st_point(c(30, 10)))

    # accessors/measures/predicates still return a bare, narrowed (one-column) result,
    # never retaining the other columns the way Group A now does.
    expect_identical(ncol(st_area(df)), 1L)
    expect_identical(ncol(st_dimension(df)), 1L)
    expect_identical(ncol(st_intersects(df, query)), 1L)
    expect_identical(ncol(st_nearest_points(df, query)), 1L)
})

test_that("chained Group A calls compose correctly on a derived geometry column", {
    df <- DuckDBDataFrame(spatial_path)
    df <- df[which(!is.na(spatial_wkt)), ]

    chained <- st_simplify(st_buffer(df, 1), dTolerance = 0.1, preserveTopology = FALSE)
    expect_identical(colnames(chained), colnames(df))
    expect_identical(nrow(chained), nrow(df))
    # The point of this test is composition (the second call's SQL nests around the first
    # call's own expression, not a bare column reference) -- not an exact cross-engine geometry
    # match, which st_buffer's own discretization already rules out above. Confirm it actually
    # differs from both the original and the single-step st_buffer() result.
    expect_false(isTRUE(all.equal(.geom_wkt(chained), .geom_wkt(df))))
    expect_false(isTRUE(all.equal(.geom_wkt(chained), .geom_wkt(st_buffer(df, 1)))))
})
