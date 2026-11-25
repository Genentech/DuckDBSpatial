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

test_that("st_bbox and st_coordinates on a DuckDBTable match sf and keep the CRS", {
    skip_if_not_installed("nanoparquet")
    pts <- st_sf(id = 1:4, val = c(1.5, 2.5, 3.5, 4.5), geometry = st_sfc(
        st_point(c(1, 2)), st_point(c(5, 9)), st_point(c(3, 4)), st_point(c(-2, 7)),
        crs = 4326))
    path <- tempfile(fileext = ".parquet")
    on.exit(unlink(path), add = TRUE)
    writeGeoParquet(pts, path)
    ddf <- readGeoParquet(path)

    # the table and its geometry column both agree with sf, CRS included
    for (b in list(st_bbox(ddf), st_bbox(ddf[["geometry"]]))) {
        expect_s3_class(b, "bbox")
        expect_equal(as.numeric(b), as.numeric(st_bbox(pts)))
        expect_equal(st_crs(b), st_crs(pts))
    }

    # an unknown CRS stays unknown
    pts_na <- pts
    st_crs(pts_na) <- NA
    path_na <- tempfile(fileext = ".parquet")
    on.exit(unlink(path_na), add = TRUE)
    writeGeoParquet(pts_na, path_na)
    expect_true(is.na(st_crs(st_bbox(readGeoParquet(path_na)))))

    # only the coordinates come back, as for an sf data frame
    xy <- as.data.frame(st_coordinates(ddf))
    expect_identical(names(xy), c("X", "Y"))
    expect_equal(xy$X, unname(st_coordinates(pts)[, "X"]))
    expect_equal(xy$Y, unname(st_coordinates(pts)[, "Y"]))

    # 'geom' picks the column when there is more than one
    two <- cbind(ddf, geom2 = ddf[["geometry"]])
    expect_error(st_bbox(two), "exactly one GEOMETRY")
    expect_error(st_coordinates(two), "exactly one GEOMETRY")
    expect_equal(as.numeric(st_bbox(two, geom = "geom2")), as.numeric(st_bbox(pts)))
    expect_equal(as.data.frame(st_coordinates(two, geom = "geom2"))$X, xy$X)
    expect_error(st_bbox(two, geom = "id"), "not a GEOMETRY column")
})

## Two geometries are the same shape when what lies in one and not the other has
## no area to speak of. sf::st_equals() asks for the same vertices to the last
## bit, and the GEOS that DuckDB bundles and the one that sf links can differ in
## the last digits of a vertex on an arc.
.same_shape <- function(got, want, tolerance = 1e-6) {
    vapply(seq_along(got), function(i) {
        diff <- sum(as.numeric(sf::st_area(sf::st_sym_difference(got[i], want[i]))))
        diff <= tolerance * max(as.numeric(sf::st_area(want[i])), 1)
    }, logical(1L))
}

## st_buffer(): the arguments of sf::st_buffer(), and st_union() with no 'y'.

.lazy_layer <- function(g, keycol = "id") {
    path <- tempfile(fileext = ".parquet")
    writeGeoParquet(g, path)
    list(path = path, layer = readGeoParquet(path, keycol = keycol))
}

test_that("st_buffer takes the end cap, join, segment and mitre arguments of sf", {
    skip_if_not_installed("nanoparquet")
    shapes <- list(
        point = sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(5, 5))),
        line = sf::st_sfc(sf::st_linestring(rbind(c(0, 0), c(10, 0), c(10, 10))),
                          sf::st_linestring(rbind(c(0, 5), c(3, 8)))),
        polygon = sf::st_sfc(
            sf::st_polygon(list(rbind(c(0, 0), c(4, 0), c(4, 4), c(0, 4), c(0, 0)))),
            sf::st_polygon(list(rbind(c(10, 10), c(12, 10), c(11, 13), c(10, 10))))))
    for (nm in names(shapes)) {
        g <- sf::st_sf(id = c("a", "b"), geometry = shapes[[nm]], crs = 3857)
        lazy <- .lazy_layer(g)
        on.exit(unlink(lazy$path), add = TRUE)
        for (cap in c("ROUND", "FLAT", "SQUARE")) {
            # sf refuses a flat cap on a point
            if (nm == "point" && cap == "FLAT")
                next
            for (join in c("ROUND", "MITRE", "BEVEL")) {
                for (nq in c(4L, 30L)) {
                    want <- sf::st_buffer(g$geometry, 1.5, nQuadSegs = nq,
                                          endCapStyle = cap, joinStyle = join,
                                          mitreLimit = 2)
                    got <- st_as_sfc(st_buffer(lazy$layer, 1.5, nQuadSegs = nq,
                                               endCapStyle = cap, joinStyle = join,
                                               mitreLimit = 2))
                    info <- paste(nm, cap, join, nq)
                    expect_identical(sf::st_crs(got), sf::st_crs(want), info = info)
                    expect_true(all(.same_shape(got, want)),
                                info = info)
                    expect_equal(as.numeric(sf::st_area(got)), as.numeric(sf::st_area(want)),
                                 tolerance = 1e-6, info = info)
                }
            }
        }
    }
})

test_that("st_buffer with a square end cap gives the square that sf gives, as for a Visium HD spot", {
    skip_if_not_installed("nanoparquet")
    g <- sf::st_sf(id = "s", geometry = sf::st_sfc(sf::st_point(c(100, 200))), crs = 3857)
    lazy <- .lazy_layer(g)
    on.exit(unlink(lazy$path))
    got <- st_as_sfc(st_buffer(lazy$layer, 4, endCapStyle = "SQUARE"))
    expect_identical(sf::st_as_text(got),
                     sf::st_as_text(sf::st_buffer(g$geometry, 4, endCapStyle = "SQUARE")))
    # the same through a column
    col <- st_as_sfc(st_buffer(lazy$layer[["geometry"]], 4, endCapStyle = "square"))
    expect_identical(sf::st_as_text(col), sf::st_as_text(got))
})

test_that("st_buffer follows the sf default of 30 segments, and checks its arguments", {
    skip_if_not_installed("nanoparquet")
    g <- sf::st_sf(id = "a", geometry = sf::st_sfc(sf::st_point(c(0, 0))), crs = 3857)
    lazy <- .lazy_layer(g)
    on.exit(unlink(lazy$path))
    x <- lazy$layer
    npoints <- function(s) nrow(sf::st_coordinates(s))
    expect_identical(npoints(st_as_sfc(st_buffer(x, 1))), npoints(sf::st_buffer(g$geometry, 1)))
    expect_identical(npoints(st_as_sfc(st_buffer(x, 1, nQuadSegs = 4))), 17L)

    # ST_Buffer has no one-sided buffer
    expect_error(st_buffer(x, 1, singleSide = TRUE), "singleSide")
    expect_error(st_buffer(x, 1, singleSide = NA), "singleSide")
    expect_error(st_buffer(x, 1, endCapStyle = "ROUNDISH"), "endCapStyle")
    expect_error(st_buffer(x, 1, endCapStyle = c("ROUND", "FLAT")), "endCapStyle")
    expect_error(st_buffer(x, 1, joinStyle = 3), "joinStyle")
    expect_error(st_buffer(x, 1, nQuadSegs = 0), "nQuadSegs")
    expect_error(st_buffer(x, 1, mitreLimit = -1), "mitreLimit")
    expect_error(st_buffer(x, c(1, 2)), "dist")
    expect_error(st_buffer(x, NA_real_), "dist")
    # an argument that would be ignored is an error
    expect_error(st_buffer(x, 1, nonsense = 2), "unused argument")
})

test_that("st_union with no y returns the union of all the geometries, with the CRS", {
    skip_if_not_installed("nanoparquet")
    cells <- sf::st_sf(id = c("a", "b", "c"), geometry = sf::st_sfc(
        sf::st_polygon(list(rbind(c(0, 0), c(2, 0), c(2, 2), c(0, 2), c(0, 0)))),
        sf::st_polygon(list(rbind(c(1, 1), c(3, 1), c(3, 3), c(1, 3), c(1, 1)))),
        sf::st_polygon(list(rbind(c(10, 10), c(11, 10), c(11, 11), c(10, 11), c(10, 10))))),
        crs = 3857)
    lazy <- .lazy_layer(cells)
    on.exit(unlink(lazy$path))
    want <- sf::st_union(cells)

    # the table, a DuckDBDataFrame of cells
    got <- st_union(lazy$layer)
    expect_s3_class(got, "sfc")
    expect_length(got, 1L)
    expect_identical(sf::st_crs(got), sf::st_crs(want))
    expect_true(.same_shape(got, want))
    expect_equal(as.numeric(sf::st_area(got)), 8)

    # the column gives the same, and used to lose the CRS
    from_col <- st_union(lazy$layer[["geometry"]])
    expect_identical(sf::st_crs(from_col), sf::st_crs(want))
    expect_true(.same_shape(from_col, want))

    # a subset of the rows is the union of those rows only
    part <- st_union(lazy$layer[c("a", "c"), ])
    expect_equal(as.numeric(sf::st_area(part)), 5)

    # the union with a y is still a lazy table
    expect_s4_class(st_union(lazy$layer, cells$geometry[[3L]]), "DuckDBDataFrame")
})

test_that("st_union then st_concave_hull gives the tissue boundary, as in a SpatialFeatureExperiment", {
    skip_if_not_installed("nanoparquet")
    set.seed(5)
    n <- 60L
    xy <- cbind(runif(n, 0, 100), runif(n, 0, 100))
    cells <- sf::st_sf(id = sprintf("c%03d", seq_len(n)),
                       geometry = sf::st_buffer(sf::st_sfc(lapply(seq_len(n), function(i) sf::st_point(xy[i, ]))), 4),
                       crs = 3857)
    lazy <- .lazy_layer(cells)
    on.exit(unlink(lazy$path))
    want <- sf::st_concave_hull(sf::st_union(sf::st_set_geometry(cells, "geometry")), ratio = 0.5)
    got <- sf::st_concave_hull(st_union(lazy$layer), ratio = 0.5)
    expect_identical(sf::st_crs(got), sf::st_crs(want))
    expect_equal(as.numeric(sf::st_area(got)), as.numeric(sf::st_area(want)), tolerance = 1e-6)
})
