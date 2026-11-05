# Tests the basic functions of a DuckDBColumn.
# library(testthat); library(DuckDBSpatial); source("setup.R"); source("test-DuckDBColumn.R")

test_that("Spatial coercion methods work as expected for a DuckDBColumn", {
    df <- DuckDBDataFrame(spatial_path)
    df <- df[which(!is.na(spatial_wkt)),]

    x <- df[["geometry"]]
    type <- df[["type"]]
    sfc <- st_as_sfc(spatial_wkt[!is.na(spatial_wkt)])

    # st_as_sfc() materializes (matching plain sf's own semantics): a round
    # trip through the still-lazy st_as_text() and back reproduces the
    # original geometry as a real in-memory sfc, not another DuckDBColumn.
    wkt <- st_as_text(x)
    roundtrip <- st_as_sfc(wkt)
    expect_s3_class(roundtrip, "sfc")
    expect_equal(st_as_text(roundtrip), st_as_text(sfc))
    expect_equal(st_as_binary(roundtrip, hex = TRUE), st_as_binary(sfc, hex = TRUE))

    checkDuckDBColumn(st_as_binary(x, hex = TRUE), st_as_binary(sfc, hex = TRUE))
    checkDuckDBColumn(st_as_text(x), st_as_text(sfc))
})

test_that("Spatial geometry accessors work as expected for a DuckDBColumn", {
    df <- DuckDBDataFrame(spatial_path)
    df <- df[which(!is.na(spatial_wkt)),]

    x <- df[["geometry"]]
    type <- df[["type"]]
    sfc <- st_as_sfc(spatial_wkt[!is.na(spatial_wkt)])

    pt_idx <- which(st_geometry_type(sfc) == "POINT" & !st_is_empty(sfc))
    checkDuckDBColumn(st_as_text(x[pt_idx]), st_as_text(sfc[pt_idx]))
    checkDuckDBColumn(st_dimension(x), rep(c(1,0,2,0,2), c(5,3,4,3,3)))
    checkDuckDBColumn(st_end_point(x), rep(c("POINT (40 40)", NA), c(1,17)))
    checkDuckDBColumn(st_geometry_type(x), as.character(st_geometry_type(sfc)))
    checkDuckDBColumn(st_is_empty(x), st_is_empty(sfc))
    checkDuckDBColumn(st_is_simple(x), st_is_simple(sfc))
    checkDuckDBColumn(st_is_valid(x), st_is_valid(sfc))
    # st_is_closed, st_is_ring have no equivalent
    line_df <- df[df$type %in% c("linestring", "multilinestring"), ]
    if (nrow(line_df) > 0L) {
        closed <- st_is_closed(line_df[["geometry"]])
        expect_s4_class(closed, "DuckDBColumn")
        expect_identical(length(closed), nrow(line_df))
        ## LINESTRING(30 10, 10 30, 40 40) is not closed
        expect_false(as.vector(closed)[1])
        ring <- st_is_ring(line_df[["geometry"]])
        expect_identical(length(ring), nrow(line_df))
    }
    checkDuckDBColumn(st_num_geometries(x), c(1,0,1,2,0,1,4,0,1,2,2,0,1,0,1,1,1,0))
    checkDuckDBColumn(st_num_interior_rings(x), rep(c(NA,0,1,0),c(15,1,1,1)))
    checkDuckDBColumn(st_num_points(x), c(3,0,3,7,0,1,4,0,5,9,14,0,1,0,1,5,9,0))
    checkDuckDBColumn(st_start_point(x), rep(c("POINT (30 10)", NA), c(1,17)))
})

test_that("Spatial measurement methods work as expected for a DuckDBColumn", {
    df <- DuckDBDataFrame(spatial_path)
    df <- df[which(!is.na(spatial_wkt)),]

    x <- df[["geometry"]]
    type <- df[["type"]]
    sfc <- st_as_sfc(spatial_wkt[!is.na(spatial_wkt)])

    checkDuckDBColumn(st_area(x), st_area(sfc))
    checkDuckDBColumn(st_distance(x, sfc[13]), st_distance(sfc, sfc[13]))
    checkDuckDBColumn(st_length(x), st_length(sfc))
    checkDuckDBColumn(st_perimeter(x), st_perimeter(sfc))
})

test_that("Spatial unary operations work as expected for a DuckDBColumn", {
    df <- DuckDBDataFrame(spatial_path)
    df <- df[which(!is.na(spatial_wkt)),]

    x <- df[["geometry"]]
    type <- df[["type"]]
    sfc <- st_as_sfc(spatial_wkt[!is.na(spatial_wkt)])

    query_pt_sfc <- st_sfc(st_point(c(30, 10)))
    query_poly_sfc <- st_sfc(st_polygon(list(
        matrix(c(0,0, 100,0, 100,100, 0,100, 0,0), ncol = 2, byrow = TRUE))))

    checkDuckDBColumn(st_boundary(x), st_boundary(sfc))
    checkDuckDBColumn(st_buffer(x, 1), st_buffer(sfc, 1))
    checkDuckDBColumn(st_build_area(x),
                      rep(c("GEOMETRYCOLLECTION EMPTY", "POLYGON ((30 10, 10 20, 20 40, 40 40, 30 10))",
                            "MULTIPOLYGON (((5 10, 10 20, 40 10, 15 5, 5 10)), ((10 40, 45 40, 30 20, 10 40)))",
                            "MULTIPOLYGON (((45 20, 30 5, 10 10, 10 30, 20 35, 45 20), (20 25, 20 15, 30 20, 20 25)), ((45 30, 20 45, 40 40, 45 30)))",
                            "GEOMETRYCOLLECTION EMPTY",
                            "POLYGON ((30 10, 10 20, 20 40, 40 40, 30 10))",
                            "POLYGON ((35 10, 10 20, 15 40, 45 45, 35 10), (20 30, 30 20, 35 35, 20 30))",
                            "GEOMETRYCOLLECTION EMPTY"), c(8,1,1,1,4,1,1,1)))
    checkDuckDBColumn(st_centroid(x), st_centroid(sfc))
    checkDuckDBColumn(st_reduce_precision(st_collection_extract(st_voronoi(x), "POLYGON")[1L], 1),
                      "MULTIPOLYGON (((13 70, 70 70, 70 13, 70 -20, -20 -20, -20 70, 13 70)))")
    checkDuckDBColumn(st_concave_hull(x, 0.5), st_concave_hull(sfc, 0.5))
    checkDuckDBColumn(st_convex_hull(x), st_convex_hull(sfc))
    checkDuckDBColumn(st_envelope(x),
                      c("POLYGON ((10 10, 40 10, 40 40, 10 40, 10 10))",
                        "POINT EMPTY",
                        "POLYGON ((10 10, 40 10, 40 40, 10 40, 10 10))",
                        "POLYGON ((10 10, 40 10, 40 40, 10 40, 10 10))",
                        "POINT EMPTY", "POINT (30 10)",
                        "POLYGON ((10 10, 40 10, 40 40, 10 40, 10 10))",
                        "POINT EMPTY",
                        "POLYGON ((10 10, 40 10, 40 40, 10 40, 10 10))",
                        "POLYGON ((5 5, 45 5, 45 40, 5 40, 5 5))",
                        "POLYGON ((10 5, 45 5, 45 45, 10 45, 10 5))",
                        "POINT EMPTY", "POINT (30 10)", "POINT EMPTY",
                        "POINT (40 40)",
                        "POLYGON ((10 10, 40 10, 40 40, 10 40, 10 10))",
                        "POLYGON ((10 10, 45 10, 45 45, 10 45, 10 10))",
                        "POINT EMPTY"))
    checkDuckDBColumn(st_exterior_ring(x),
                      c(rep(NA, 15), "LINESTRING (30 10, 40 40, 20 40, 10 20, 30 10)",
                        "LINESTRING (35 10, 45 45, 15 40, 10 20, 35 10)", "LINESTRING EMPTY"))
    checkDuckDBColumn(st_flip_coordinates(x),
                      c("LINESTRING (10 30, 30 10, 40 40)",
                        "LINESTRING EMPTY",
                        "MULTILINESTRING ((10 30, 30 10, 40 40))",
                        "MULTILINESTRING ((10 10, 20 20, 40 10), (40 40, 30 30, 20 40, 10 30))",
                        "MULTILINESTRING EMPTY",
                        "MULTIPOINT (10 30)",
                        "MULTIPOINT (40 10, 30 40, 20 20, 10 30)",
                        "MULTIPOINT EMPTY",
                        "MULTIPOLYGON (((10 30, 40 40, 40 20, 20 10, 10 30)))",
                        "MULTIPOLYGON (((20 30, 40 45, 40 10, 20 30)), ((5 15, 10 40, 20 10, 10 5, 5 15)))",
                        "MULTIPOLYGON (((40 40, 45 20, 30 45, 40 40)), ((35 20, 30 10, 10 10, 5 30, 20 45, 35 20), (20 30, 15 20, 25 20, 20 30)))",
                        "MULTIPOLYGON EMPTY",
                        "POINT (10 30)",
                        "POINT EMPTY",
                        "POINT (40 40)",
                        "POLYGON ((10 30, 40 40, 40 20, 20 10, 10 30))",
                        "POLYGON ((10 35, 45 45, 40 15, 20 10, 10 35), (30 20, 35 35, 20 30, 30 20))",
                        "POLYGON EMPTY"))
    checkDuckDBColumn(st_reduce_precision(st_inscribed_circle(x[12L], 0.01), 1),
                      "POLYGON ((34 23, 34 21, 33 19, 31 17, 30 16, 28 15, 26 14, 24 14, 22 14, 20 15, 18 16, 17 17, 15 19, 14 21, 14 23, 14 25, 14 27, 14 29, 15 30, 17 32, 18 33, 20 34, 22 35, 24 35, 26 35, 28 34, 30 33, 31 32, 33 30, 34 29, 34 27, 34 25, 34 23))")
    checkDuckDBColumn(st_line_interpolate(df[df$type == "linestring", "geometry"][1], 5, normalized = FALSE),
                      st_line_interpolate(sfc[1], 5, normalized = FALSE))
    checkDuckDBColumn(st_line_interpolate(df[df$type == "linestring", "geometry"][1], 0.5, normalized = TRUE),
                      st_line_interpolate(sfc[1], 0.5, normalized = TRUE))
    checkDuckDBColumn(st_line_merge(df[df$type == "multilinestring", "geometry"]),
                      st_line_merge(sfc[3:5]))
    checkDuckDBColumn(st_line_merge(df[df$type == "multilinestring", "geometry"], directed = TRUE),
                      st_line_merge(sfc[3:5], directed = TRUE))
    checkDuckDBColumn(st_line_project(df[df$type == "linestring", "geometry"][1], query_pt_sfc),
                      st_line_project(sfc[1], query_pt_sfc))
    checkDuckDBColumn(st_line_project(df[df$type == "linestring", "geometry"][1], query_pt_sfc, normalized = TRUE),
                      st_line_project(sfc[1], query_pt_sfc, normalized = TRUE))
    checkDuckDBColumn(st_reduce_precision(st_line_substring(x[1L], 0.25, 0.75), 1), "LINESTRING (19 21, 10 30, 26 35)")
    checkDuckDBColumn(st_make_valid(x), st_make_valid(sfc))
    checkDuckDBColumn(st_minimum_rotated_rectangle(x), st_minimum_rotated_rectangle(sfc))
    line_idx <- which(st_geometry_type(sfc) == "LINESTRING" & !st_is_empty(sfc))[1L]
    if (!is.na(line_idx)) {
        checkDuckDBColumn(st_node(df[line_idx, "geometry"]), st_node(sfc[line_idx]))
    }
    checkDuckDBColumn(st_normalize(x), st_normalize(sfc))
    checkDuckDBColumn(st_point_on_surface(x), st_point_on_surface(sfc))
    checkDuckDBColumn(st_reverse(x), st_reverse(sfc))
    checkDuckDBColumn(st_remove_repeated_points(x), as.vector(x))
    checkDuckDBColumn(st_simplify(x), st_simplify(sfc))
    checkDuckDBColumn(st_voronoi(x), st_voronoi(sfc))
})

test_that("Spatial binary spatial predicates work as expected for a DuckDBColumn", {
    df <- DuckDBDataFrame(spatial_path)
    df <- df[which(!is.na(spatial_wkt)),]

    x <- df[["geometry"]]
    type <- df[["type"]]
    sfc <- st_as_sfc(spatial_wkt[!is.na(spatial_wkt)])

    query_pt_sfc <- st_sfc(st_point(c(30, 10)))
    query_poly_sfc <- st_sfc(st_polygon(list(
        matrix(c(0,0, 100,0, 100,100, 0,100, 0,0), ncol = 2, byrow = TRUE))))
    query_line_sfc <- st_sfc(st_linestring(
        matrix(c(0,0, 50,50), ncol = 2, byrow = TRUE)))
    query_overlap_sfc <- st_sfc(st_polygon(list(
        matrix(c(25,5, 50,50, 5,25, 25,5), ncol = 2, byrow = TRUE))))
    far_pt_sfc <- st_sfc(st_point(c(999, 999)))

    checkDuckDBColumn(st_contains(x, query_pt_sfc), st_contains(sfc, query_pt_sfc))
    checkDuckDBColumn(st_contains_properly(x, query_pt_sfc), st_contains_properly(sfc, query_pt_sfc))
    checkDuckDBColumn(st_covered_by(x, query_poly_sfc), st_covered_by(sfc, query_poly_sfc))
    checkDuckDBColumn(st_covers(x, query_pt_sfc), st_covers(sfc, query_pt_sfc))
    checkDuckDBColumn(st_crosses(x, query_line_sfc), st_crosses(sfc, query_line_sfc))
    checkDuckDBColumn(st_disjoint(x, far_pt_sfc), st_disjoint(sfc, far_pt_sfc))
    checkDuckDBColumn(st_equals(x, query_pt_sfc), st_equals(sfc, query_pt_sfc))
    checkDuckDBColumn(st_intersects(x, query_pt_sfc), st_intersects(sfc, query_pt_sfc))
    checkDuckDBColumn(st_is_within_distance(x, query_pt_sfc, 5), st_is_within_distance(sfc, query_pt_sfc, dist = 5))
    checkDuckDBColumn(st_overlaps(x, query_overlap_sfc), st_overlaps(sfc, query_overlap_sfc))
    checkDuckDBColumn(st_touches(x, query_pt_sfc), st_touches(sfc, query_pt_sfc))
    checkDuckDBColumn(st_within(x, query_poly_sfc), st_within(sfc, query_poly_sfc))
    checkDuckDBColumn(st_within_properly(x, query_poly_sfc), st_within_properly(sfc, query_poly_sfc))
})

test_that("Spatial binary set operations work as expected for a DuckDBColumn", {
   df <- DuckDBDataFrame(spatial_path)
    df <- df[which(!is.na(spatial_wkt)),]

    x <- df[["geometry"]]
    type <- df[["type"]]
    sfc <- st_as_sfc(spatial_wkt[!is.na(spatial_wkt)])

    query_pt_sfc <- st_sfc(st_point(c(30, 10)))
    query_poly_sfc <- st_sfc(st_polygon(list(
        matrix(c(0,0, 100,0, 100,100, 0,100, 0,0), ncol = 2, byrow = TRUE))))

    checkDuckDBColumn(st_difference(x, query_pt_sfc)[1], "LINESTRING (30 10, 10 30, 40 40)")
    checkDuckDBColumn(st_intersection(x, query_poly_sfc)[1], "LINESTRING (30 10, 10 30, 40 40)")
    checkDuckDBColumn(st_nearest_points(x, query_pt_sfc), st_nearest_points(sfc, query_pt_sfc))
    checkDuckDBColumn(st_union(x, query_pt_sfc), st_union(sfc, query_pt_sfc))
})

test_that("Spatial aggregate operations work as expected for a DuckDBColumn", {
    df <- DuckDBDataFrame(spatial_path)
    df <- df[which(!is.na(spatial_wkt)),]

    x <- df[["geometry"]]
    type <- df[["type"]]
    sfc <- st_as_sfc(spatial_wkt[!is.na(spatial_wkt)])

    bbox_result <- st_bbox(x)
    expect_s3_class(bbox_result, "bbox")
    expect_equal(as.numeric(bbox_result), as.numeric(st_bbox(sfc)))

    collect_result <- st_collect(x)
    expect_s3_class(collect_result, "sfc")
    expect_length(collect_result, 1L)

    union_result <- st_union(x)
    expect_s3_class(union_result, "sfc")
    expect_length(union_result, 1L)
})

test_that("Spatial SQL helpers work as expected for a DuckDBTable", {
    env <- st_make_envelope_sql(0, 0, 1, 1)
    expect_s3_class(env, "sql")
    expect_match(as.character(env), "ST_MakeEnvelope")

    pt <- st_point_sql("x", "y")
    expect_s3_class(pt, "sql")
    expect_match(as.character(pt), "ST_Point")

    wkt <- st_geomfromtext_sql("POINT (0 0)")
    expect_s3_class(wkt, "sql")
    expect_match(as.character(wkt), "ST_GeomFromText")
})

test_that("hasGeometry detects a GEOMETRY-typed column without materializing a query", {
    shapes <- DuckDBDataFrame(spatial_path)
    expect_true(hasGeometry(shapes))
    expect_false(hasGeometry(shapes, "type"))
    expect_false(hasGeometry(shapes, "nonexistent_column"))

    # A table with no geometry column at all (the motivating case: calling
    # a geometry accessor on this applies its SQL predicate to every
    # column and only errors once materialized, not when built).
    non_spatial_path <- tempfile(fileext = ".parquet")
    on.exit(unlink(non_spatial_path), add = TRUE)
    arrow::write_parquet(data.frame(a = 1:3, b = c("x", "y", "z")), non_spatial_path)
    plain <- DuckDBDataFrame(non_spatial_path)
    expect_false(hasGeometry(plain))

    expect_error(hasGeometry(data.frame(x = 1)), "DuckDBTable or DuckDBDataFrame")
})

test_that("Spatial table joins work as expected for a DuckDBTable", {
    df <- DuckDBDataFrame(spatial_path)
    df <- df[which(!is.na(spatial_wkt)), ]

    joined <- st_join(df, df)
    expect_s4_class(joined, "DuckDBDataFrame")
    expect_true(nrow(joined) >= nrow(df))
})

test_that("st_join_table returns a sparse id_x/id_y pair table for two lazy tables", {
    df <- DuckDBDataFrame(spatial_path)
    df <- df[which(!is.na(spatial_wkt)), ]

    pairs <- st_join_table(df, df)
    expect_s4_class(pairs, "DuckDBDataFrame")
    pairs_df <- as.data.frame(pairs)
    expect_setequal(colnames(pairs_df), c("id_x", "id_y"))
    # Same predicate, same tables: must have exactly as many pairs as the
    # already-established full-attribute st_join(df, df) above.
    expect_equal(nrow(pairs_df), nrow(as.data.frame(st_join(df, df))))

    # as.data.frame() on a lazy table materializes its geometry column as a
    # list of raw WKB (DuckDBDataFrame's own on-disk shape), which must round-
    # trip back into a working join rather than erroring on the type.
    mem_pairs <- as.data.frame(st_join_table(df, as.data.frame(df)))
    expect_equal(nrow(mem_pairs), nrow(pairs_df))

    expect_error(st_join_table(df, data.frame(a = 1:3)), "'geometry' column")
})

test_that("st_join/st_join_table accept an in-memory sf object on either side", {
    df <- DuckDBDataFrame(spatial_path)
    df <- df[which(!is.na(spatial_wkt)), ]

    poly_sf <- st_sf(id = 1:2, geometry = st_sfc(
        st_polygon(list(matrix(c(0, 0, 6, 0, 6, 6, 0, 6, 0, 0), ncol = 2, byrow = TRUE))),
        st_polygon(list(matrix(c(20, 5, 40, 5, 40, 15, 20, 15, 20, 5), ncol = 2, byrow = TRUE)))))

    lazy_mem <- as.data.frame(st_join_table(df, poly_sf))
    mem_lazy <- as.data.frame(st_join_table(poly_sf, df))
    expect_equal(nrow(lazy_mem), nrow(mem_lazy))
    expect_true(nrow(lazy_mem) > 0L)

    mem_mem <- as.data.frame(st_join_table(poly_sf, poly_sf))
    expect_equal(nrow(mem_mem), 2L)

    joined <- st_join(df, poly_sf)
    expect_s4_class(joined, "DuckDBDataFrame")
    expect_equal(nrow(joined), nrow(lazy_mem))
})

test_that("st_join_table tolerates NULL/missing geometries in a materialized WKB column", {
    # as.data.frame() on an unfiltered DuckDBDataFrame legitimately contains
    # NULL geometries; these must become empty geometries (never matching
    # anything) rather than crashing st_as_sfc.WKB()'s all-raw assertion.
    mem_df <- as.data.frame(DuckDBDataFrame(spatial_path))
    expect_true(any(vapply(mem_df$geometry, is.null, logical(1))))

    poly_sf <- st_sf(id = 1, geometry = st_sfc(st_polygon(list(
        matrix(c(0, 0, 40, 0, 40, 40, 0, 40, 0, 0), ncol = 2, byrow = TRUE)))))
    pairs <- as.data.frame(st_join_table(mem_df, poly_sf))
    expect_true(all(pairs$id_x %in% seq_len(nrow(mem_df))))
})

test_that("st_join_table accepts a plain WKT-text geometry column", {
    # MultiAssaySpatialExperiment's own idiomatic DataFrame shape (see its
    # spatialJoin,DataFrame,DataFrame-method) stores geometry as WKT text,
    # not sfc or WKB.
    df <- DuckDBDataFrame(spatial_path)
    df <- df[which(!is.na(spatial_wkt)), ]

    wkt_df <- data.frame(id = 1:2, geometry = c(
        "POLYGON((0 0, 6 0, 6 6, 0 6, 0 0))",
        "POLYGON((20 5, 40 5, 40 15, 20 15, 20 5))"))
    pairs <- as.data.frame(st_join_table(df, wkt_df))
    expect_true(nrow(pairs) > 0L)

    # A NULL/NA geometry must become an empty geometry (never matching
    # anything) rather than crashing st_as_sfc() on the missing value.
    wkt_df_na <- data.frame(id = 1:3, geometry = c(
        "POLYGON((0 0, 6 0, 6 6, 0 6, 0 0))", NA_character_,
        "POLYGON((20 5, 40 5, 40 15, 20 15, 20 5))"))
    pairs_na <- as.data.frame(st_join_table(df, wkt_df_na))
    expect_false(2L %in% pairs_na$id_y)

    expect_error(st_join_table(df, data.frame(id = 1, geometry = "not a geometry")),
                "cannot parse WKT geometry column")
})

test_that(".geom_to_sql()'s redirect errors reference real DuckDBSpatial functions", {
    df <- DuckDBDataFrame(spatial_path)
    df <- df[which(!is.na(spatial_wkt)), ]

    expect_error(sf::st_intersects(df, df), "layerSpatialMatch")
    expect_error(sf::st_intersects(df, st_sfc(st_point(c(0, 0)), st_point(c(1, 1)))),
                "layerSpatialMatch")
})

test_that("st_join_table does not collide with a pre-existing id_x/id_y data column", {
    df <- DuckDBDataFrame(spatial_path)
    df <- df[which(!is.na(spatial_wkt)), ]
    conn <- DuckDBDataFrame::dbconn(df)
    q <- dbplyr::sql_render(DuckDBDataFrame::tblconn(df, select = FALSE))
    # A real data column named id_x/id_y (same names st_join_table's output
    # uses) must not shadow the row_number() it assigns internally.
    tagged <- DuckDBDataFrame(dplyr::tbl(conn, dplyr::sql(
        sprintf("SELECT *, 999 AS id_x, 999 AS id_y FROM (%s)", q))))

    pairs <- as.data.frame(st_join_table(tagged, tagged))
    expect_false(any(pairs$id_x == 999L))
    expect_true(all(pairs$id_x %in% seq_len(nrow(tagged))))
    expect_true(all(pairs$id_y %in% seq_len(nrow(tagged))))
})

test_that("st_join/st_join_table cast a plain WKT-text geometry column on either side, not just a freshly-uploaded one", {
    # .as_duckdb_table() detects and casts a WKT-text geometry column, but
    # only when uploading a fresh in-memory object for the first time; it's
    # a no-op for anything that's already a DuckDBTable/DuckDBDataFrame.
    # 'x' here (unlike the plain-data.frame 'y' in the test above) is
    # exactly that already-a-table case, built with datacols= the same way
    # a CSV-backed layer would be.
    pts_wkt <- c("POINT(1 1)", "POINT(11 11)", "POINT(99 99)")
    csv_path <- tempfile(fileext = ".csv")
    on.exit(unlink(csv_path), add = TRUE)
    write.csv(data.frame(id = 1:3, geometry = pts_wkt, stringsAsFactors = FALSE),
              csv_path, row.names = FALSE)
    x_text <- DuckDBDataFrame(csv_path, datacols = c("id", "geometry"))
    expect_false(hasGeometry(x_text))

    df <- DuckDBDataFrame(spatial_path)
    df <- df[which(!is.na(spatial_wkt)), ]

    pts_sfc <- st_as_sfc(pts_wkt)
    shapes_sfc <- st_as_sfc(spatial_wkt[!is.na(spatial_wkt)])
    gt_pairs <- sum(lengths(st_intersects(pts_sfc, shapes_sfc)))
    expect_true(gt_pairs > 0L)

    pairs <- as.data.frame(st_join_table(x_text, df))
    expect_equal(nrow(pairs), gt_pairs)

    joined <- st_join(x_text, df)
    expect_s4_class(joined, "DuckDBDataFrame")
    expect_equal(nrow(joined), gt_pairs)

    # Same asymmetry the other way round: 'y' already a DuckDBTable with
    # WKT-text geometry (as opposed to the plain-data.frame 'y' case already
    # covered, which goes through .as_duckdb_table()'s own cast).
    pairs_rev <- as.data.frame(st_join_table(df, x_text))
    expect_equal(nrow(pairs_rev), gt_pairs)
})

test_that(".geom_col_is_native()/.geom_col_sql_ref() detect native GEOMETRY without double-casting", {
    df <- DuckDBDataFrame(spatial_path)
    df <- df[which(!is.na(spatial_wkt)), ]
    conn <- dbconn(df)
    q <- dbplyr::sql_render(tblconn(df, select = FALSE))

    expect_true(DuckDBSpatial:::.geom_col_is_native(conn, q, "geometry"))
    ref <- DuckDBSpatial:::.geom_col_sql_ref(conn, q, "geometry", prefix = "t")
    expect_equal(ref, 't."geometry"')
    expect_false(grepl("ST_GeomFromText", ref, fixed = TRUE))

    csv_path <- tempfile(fileext = ".csv")
    on.exit(unlink(csv_path), add = TRUE)
    write.csv(data.frame(id = 1, geometry = "POINT(1 1)", stringsAsFactors = FALSE),
              csv_path, row.names = FALSE)
    text_df <- DuckDBDataFrame(csv_path, datacols = c("id", "geometry"))
    conn2 <- dbconn(text_df)
    q2 <- dbplyr::sql_render(tblconn(text_df, select = FALSE))
    expect_false(DuckDBSpatial:::.geom_col_is_native(conn2, q2, "geometry"))
    ref2 <- DuckDBSpatial:::.geom_col_sql_ref(conn2, q2, "geometry", prefix = "t")
    expect_equal(ref2, 'ST_GeomFromText(t."geometry")')

    # A GEOMETRY column round-tripped through GeoParquet carries its CRS as
    # part of the type itself (e.g. GEOMETRY('OGC:CRS84')); must still be
    # recognized as native, not just bare "GEOMETRY".
    expect_true(DuckDBSpatial:::.is_geometry_type("GEOMETRY('OGC:CRS84')"))
    expect_true(DuckDBSpatial:::.is_geometry_type("geometry"))
    expect_false(DuckDBSpatial:::.is_geometry_type("VARCHAR"))

    # Empty subquery: a LIMIT 1 probe has nothing to evaluate typeof()
    # against, so this must fall back to a schema-only DESCRIBE rather than
    # silently (and incorrectly) reporting "not geometry".
    empty_df <- df[integer(0), , drop = FALSE]
    q_empty <- dbplyr::sql_render(tblconn(empty_df, select = FALSE))
    expect_true(DuckDBSpatial:::.geom_col_is_native(dbconn(empty_df), q_empty, "geometry"))
})

test_that("st_join maps the sf predicate to its ST_* SQL function", {
    resolve <- DuckDBSpatial:::.st_predicate_sql_name
    expect_equal(resolve(sf::st_intersects), "ST_Intersects")
    expect_equal(resolve(sf::st_within), "ST_Within")
    expect_equal(resolve(sf::st_contains), "ST_Contains")
    expect_equal(resolve(sf::st_covers), "ST_Covers")
    expect_equal(resolve(sf::st_touches), "ST_Touches")
    # An unrecognized predicate errors rather than silently using ST_Intersects.
    expect_error(resolve(function(x, y) TRUE), "not recognized")
})

test_that("Spatial table filters work as expected for a DuckDBTable", {
    df <- DuckDBDataFrame(spatial_path)
    df <- df[which(!is.na(spatial_wkt)), ]

    query_pt <- st_sfc(st_point(c(30, 10)))
    filtered <- st_filter(df, query_pt)
    expect_s4_class(filtered, "DuckDBDataFrame")
    hits <- as.logical(as.vector(st_intersects(df[["geometry"]], query_pt)))
    expect_equal(nrow(filtered), sum(hits, na.rm = TRUE))
})

test_that("Spatial intersect table works as expected for a DuckDBTable", {
    df <- DuckDBDataFrame(spatial_path)
    df <- df[which(!is.na(spatial_wkt)), ]

    query_pt <- st_sfc(st_point(c(30, 10)))
    result <- st_intersects_table(df, query_pt)
    expect_s4_class(result, "DuckDBDataFrame")
    hits <- as.logical(as.vector(st_intersects(df[["geometry"]], query_pt)))
    expect_equal(nrow(result), sum(hits, na.rm = TRUE))
})

test_that("Spatial aggregate SQL helpers work as expected for a DuckDBColumn", {
    df <- DuckDBDataFrame(spatial_path)
    df <- df[which(!is.na(spatial_wkt)), ]

    x <- df[["geometry"]]
    sfc <- st_as_sfc(spatial_wkt[!is.na(spatial_wkt)])

    union_wkt <- st_as_text(st_union(sfc))
    collect_wkt <- st_as_text(st_combine(sfc))
    envelope_wkt <- st_as_text(st_as_sfc(st_bbox(sfc)))

    checkDuckDBColumn(st_union_agg(x), union_wkt)
    checkDuckDBColumn(st_collect_agg(x), collect_wkt)
    checkDuckDBColumn(st_envelope_agg(x), envelope_wkt)
})

test_that("Layer spatial engines work as expected for a DuckDBDataFrame", {
    skip_if_not_installed("arrow")
    df_pts <- data.frame(x = c(1, 5, 10, 50), y = c(1, 5, 10, 50))
    path <- tempfile(fileext = ".parquet")
    on.exit(unlink(path), add = TRUE)
    arrow::write_parquet(df_pts, path)
    pts <- DuckDBDataFrame(path, datacols = c("x", "y"))
    poly <- st_as_sfc("POLYGON((0 0, 6 0, 6 6, 0 6, 0 0))")
    pts_sfc <- st_as_sfc(paste0("POINT(", df_pts$x, " ", df_pts$y, ")"))
    env <- st_sfc(st_polygon(list(
        matrix(c(0, 0, 10, 0, 10, 10, 0, 10, 0, 0), ncol = 2, byrow = TRUE)
    )))

    lazy <- layerSpatialOverlaps(pts, poly, coords = c("x", "y"))
    expect_identical(unname(lazy),
                     as.logical(lengths(st_intersects(pts_sfc, poly)) > 0L))

    idx <- layerSubsetByBbox(pts, 0, 10, 0, 10, x_col = "x", y_col = "y")
    expect_equal(idx, which(as.logical(lengths(st_intersects(pts_sfc, env)) > 0L)))

    # layerBboxRange: the prunable range-predicate sibling — a lazy DuckDBDataFrame
    # with the same box membership as the ST_Intersects index path above.
    rng <- layerBboxRange(pts, 0, 10, 0, 10, x_col = "x", y_col = "y")
    expect_s4_class(rng, "DuckDBDataFrame")
    rng_df <- as.data.frame(rng)
    expect_equal(nrow(rng_df), length(idx))
    expect_true(all(rng_df$x >= 0 & rng_df$x <= 10 & rng_df$y >= 0 & rng_df$y <= 10))
    expect_error(layerBboxRange(pts, 0, 10, 0, 10, x_col = "nope"), "coordinate columns")

    # 3-D viewport (#95): zmin/zmax AND a `z BETWEEN` term onto the box.
    set.seed(3L)
    df3 <- data.frame(x = runif(500, 0, 100), y = runif(500, 0, 100),
                      z = runif(500, 0, 100))
    path3 <- tempfile(fileext = ".parquet")
    on.exit(unlink(path3), add = TRUE)
    arrow::write_parquet(df3, path3)
    pts3 <- DuckDBDataFrame(path3, datacols = c("x", "y", "z"))
    got3 <- as.data.frame(
        layerBboxRange(pts3, 0, 50, 0, 50, zmin = 20, zmax = 70, z_col = "z"))
    exp3 <- with(df3, x >= 0 & x <= 50 & y >= 0 & y <= 50 & z >= 20 & z <= 70)
    expect_equal(nrow(got3), sum(exp3))
    expect_true(all(got3$z >= 20 & got3$z <= 70))
    # only one z bound -> stays 2-D (z ignored, no error even though z exists)
    exp2 <- with(df3, x >= 0 & x <= 50 & y >= 0 & y <= 50)
    expect_equal(nrow(as.data.frame(layerBboxRange(pts3, 0, 50, 0, 50, zmin = 20))),
                 sum(exp2))
    # a z range without a z column errors
    pts_xy <- DuckDBDataFrame(path3, datacols = c("x", "y"))
    expect_error(layerBboxRange(pts_xy, 0, 50, 0, 50, zmin = 20, zmax = 70),
                 "coordinate columns")

    poly_idx <- layerSubsetByGeometry(pts, poly, coords = c("x", "y"))
    expect_equal(poly_idx, which(as.logical(lengths(st_intersects(pts_sfc, poly)) > 0L)))

    shapes_sfc <- st_as_sfc(spatial_wkt[!is.na(spatial_wkt)])
    matched_exp <- vapply(st_intersects(pts_sfc, shapes_sfc), function(h) {
        if (length(h) > 0L) min(h) else NA_integer_
    }, integer(1L))

    shapes_df <- DuckDBDataFrame(spatial_path)
    shapes_df <- shapes_df[which(!is.na(spatial_wkt)), ]
    matched <- layerSpatialMatch(pts, shapes_df, coords = c("x", "y"))
    expect_identical(matched, matched_exp)

    mem_tbl <- S4Vectors::DataFrame(geometry = shapes_sfc)
    matched_mem <- layerSpatialMatch(pts, mem_tbl, coords = c("x", "y"))
    expect_identical(matched_mem, matched_exp)
})

test_that("layerSpatialMatch/.spatialMatch_sql cast a plain WKT-text geometry column", {
    # Unlike the GeoParquet-backed 'shapes_df' used above, a DuckDBDataFrame
    # built with datacols= from a CSV (or any other non-GeoParquet source)
    # is a perfectly ordinary way to end up with a plain WKT-text "geometry"
    # column rather than a DuckDB-native GEOMETRY one; .spatialMatch_sql()
    # never routes 'table' through .as_duckdb_table()'s WKT-detection cast.
    shapes_wkt <- c("POLYGON((0 0, 6 0, 6 6, 0 6, 0 0))",
                    "POLYGON((20 5, 40 5, 40 15, 20 15, 20 5))")
    shapes_csv <- tempfile(fileext = ".csv")
    on.exit(unlink(shapes_csv), add = TRUE)
    write.csv(data.frame(id = 1:2, geometry = shapes_wkt, stringsAsFactors = FALSE),
              shapes_csv, row.names = FALSE)
    shapes_text <- DuckDBDataFrame(shapes_csv, datacols = c("id", "geometry"))
    expect_false(hasGeometry(shapes_text))

    pts_csv <- tempfile(fileext = ".csv")
    on.exit(unlink(pts_csv), add = TRUE)
    write.csv(data.frame(x = c(1, 5, 10, 50), y = c(1, 5, 10, 50)), pts_csv,
              row.names = FALSE)
    pts <- DuckDBDataFrame(pts_csv, datacols = c("x", "y"))

    matched <- layerSpatialMatch(pts, shapes_text, coords = c("x", "y"))

    pts_sfc <- st_as_sfc(paste0("POINT(", c(1, 5, 10, 50), " ", c(1, 5, 10, 50), ")"))
    shapes_sfc <- st_as_sfc(shapes_wkt)
    matched_exp <- vapply(st_intersects(pts_sfc, shapes_sfc), function(h) {
        if (length(h) > 0L) min(h) else NA_integer_
    }, integer(1L))
    expect_identical(matched, matched_exp)
})

test_that("layerSpatialOverlaps casts a plain WKT-text geometry column", {
    shapes_wkt <- c("POLYGON((0 0, 4 0, 4 4, 0 4, 0 0))",
                    "POLYGON((10 10, 14 10, 14 14, 10 14, 10 10))")
    shapes_csv <- tempfile(fileext = ".csv")
    on.exit(unlink(shapes_csv), add = TRUE)
    write.csv(data.frame(id = 1:2, geometry = shapes_wkt, stringsAsFactors = FALSE),
              shapes_csv, row.names = FALSE)
    shapes_text <- DuckDBDataFrame(shapes_csv, datacols = c("id", "geometry"))
    expect_false(hasGeometry(shapes_text))

    query_poly <- st_as_sfc("POLYGON((-1 -1, 5 -1, 5 5, -1 5, -1 -1))")
    overlaps <- layerSpatialOverlaps(shapes_text, query_poly)

    shapes_sfc <- st_as_sfc(shapes_wkt)
    expect_identical(unname(overlaps),
                     as.logical(lengths(st_intersects(shapes_sfc, query_poly)) > 0L))
})

test_that("GeoParquet I/O helpers work as expected", {
    conn <- DBI::dbConnect(duckdb::duckdb())
    on.exit(DBI::dbDisconnect(conn, shutdown = TRUE), add = TRUE)
    expect_invisible(enableGeoParquetConversion(conn))

    skip_if_not_installed("nanoparquet")
    pts <- st_sfc(st_point(0:1), st_point(2:3))
    sf_pt <- st_sf(id = 1:2, geometry = pts)
    path <- tempfile(fileext = ".parquet")
    on.exit(unlink(path), add = TRUE)
    writeGeoParquet(sf_pt, path)
    ddb <- readGeoParquet(path)
    expect_s4_class(ddb, "DuckDBDataFrame")
    expect_equal(nrow(ddb), 2L)
})

test_that("readGeoParquet warns when the file has no geometry column", {
    non_spatial_path <- tempfile(fileext = ".parquet")
    on.exit(unlink(non_spatial_path), add = TRUE)
    arrow::write_parquet(data.frame(a = 1:3, b = c("x", "y", "z")), non_spatial_path)

    expect_warning(
        ddb <- readGeoParquet(non_spatial_path),
        "no 'geometry' GEOMETRY column found"
    )
    expect_s4_class(ddb, "DuckDBDataFrame")
    expect_equal(nrow(ddb), 3L)

    expect_warning(readGeoParquet(non_spatial_path, geom = "geom"), "no 'geom' GEOMETRY column found")
    expect_no_warning(readGeoParquet(spatial_path))
})

test_that("st_as_sfc materializes a column already stored as GEOMETRY", {
    skip_if_not_installed("nanoparquet")
    pts <- st_sfc(st_point(0:1), st_point(2:3), crs = 4326)
    path <- tempfile(fileext = ".parquet")
    on.exit(unlink(path), add = TRUE)
    writeGeoParquet(pts, path)

    # st_as_sfc()/st_as_sf() always materialize in plain sf itself, so the
    # DuckDBTable methods must too -- not build ST_GeomFromText() SQL (which
    # expects VARCHAR/BLOB input and errors with a binder mismatch on an
    # already-GEOMETRY column) and not stay lazy either.
    ddb <- readGeoParquet(path)
    expect_identical(unname(coltypes(ddb)), "geometry")
    result <- expect_silent(st_as_sfc(ddb))
    expect_s3_class(result, "sfc")
    expect_equal(st_as_text(result), st_as_text(pts))
    expect_true(st_crs(result) == st_crs(4326))

    expect_identical(st_as_sfc(ddb[["geometry"]]), result)
})

test_that("st_as_sfc materializes WKT/WKB/GeoJSON columns not yet GEOMETRY-typed", {
    skip_if_not_installed("arrow")
    wkt_path <- tempfile(fileext = ".parquet")
    on.exit(unlink(wkt_path), add = TRUE)
    arrow::write_parquet(data.frame(geometry = c("POINT (0 0)", "POINT (1 1)")), wkt_path)
    wkt_ddb <- DuckDBDataFrame(wkt_path)
    expect_false(hasGeometry(wkt_ddb))
    result <- st_as_sfc(wkt_ddb)
    expect_s3_class(result, "sfc")
    expect_equal(st_as_text(result), c("POINT (0 0)", "POINT (1 1)"))

    skip_if_not_installed("nanoparquet")
    pts <- st_sfc(st_point(0:1), st_point(2:3))
    wkb_path <- tempfile(fileext = ".parquet")
    on.exit(unlink(wkb_path), add = TRUE)
    nanoparquet::write_parquet(data.frame(geometry = I(lapply(pts, st_as_binary))), wkb_path)
    wkb_ddb <- DuckDBDataFrame(wkb_path)
    result_wkb <- st_as_sfc(wkb_ddb, WKB = TRUE)
    expect_s3_class(result_wkb, "sfc")
    expect_equal(st_as_text(result_wkb), st_as_text(pts))

    gj_path <- tempfile(fileext = ".parquet")
    on.exit(unlink(gj_path), add = TRUE)
    arrow::write_parquet(data.frame(geometry = c(
        '{"type":"Point","coordinates":[0,1]}',
        '{"type":"Point","coordinates":[2,3]}')), gj_path)
    gj_ddb <- DuckDBDataFrame(gj_path)
    result_gj <- st_as_sfc(gj_ddb, GeoJSON = TRUE)
    expect_s3_class(result_gj, "sfc")
    expect_equal(st_as_text(result_gj), st_as_text(pts))

    expect_error(st_as_sfc(wkt_ddb, geom = "nonexistent"), "not a column")
})

test_that("st_as_sfc substitutes empty geometry for NA cells in WKT/GeoJSON columns", {
    skip_if_not_installed("arrow")
    # sf::st_as_sfc() errors outright on a bare NA ("missing value where
    # TRUE/FALSE needed") for both WKT and GeoJSON text, unlike the WKB path
    # (covered separately via st_as_sf()'s own NULL-cell test).
    wkt_path <- tempfile(fileext = ".parquet")
    on.exit(unlink(wkt_path), add = TRUE)
    arrow::write_parquet(data.frame(geometry = c("POINT (0 0)", NA, "POINT (1 1)")), wkt_path)
    wkt_ddb <- DuckDBDataFrame(wkt_path)
    result <- st_as_sfc(wkt_ddb)
    expect_identical(st_is_empty(result), c(FALSE, TRUE, FALSE))
    expect_equal(st_as_text(result[c(1L, 3L)]), c("POINT (0 0)", "POINT (1 1)"))

    gj_path <- tempfile(fileext = ".parquet")
    on.exit(unlink(gj_path), add = TRUE)
    arrow::write_parquet(data.frame(geometry = c(
        '{"type":"Point","coordinates":[0,1]}', NA,
        '{"type":"Point","coordinates":[2,3]}')), gj_path)
    gj_ddb <- DuckDBDataFrame(gj_path)
    result_gj <- st_as_sfc(gj_ddb, GeoJSON = TRUE)
    expect_identical(st_is_empty(result_gj), c(FALSE, TRUE, FALSE))
})

test_that("st_as_sfc lets an explicit crs override an already-GEOMETRY column's CRS", {
    skip_if_not_installed("nanoparquet")
    pts <- st_sfc(st_point(0:1), st_point(2:3), crs = 4326)
    path <- tempfile(fileext = ".parquet")
    on.exit(unlink(path), add = TRUE)
    writeGeoParquet(pts, path)
    ddb <- readGeoParquet(path)

    expect_true(st_crs(st_as_sfc(ddb)) == st_crs(4326))
    expect_true(st_crs(st_as_sfc(ddb, crs = 3857)) == st_crs(3857))
})

test_that("st_as_sf materializes one feature per row, not a combined geometry", {
    skip_if_not_installed("nanoparquet")
    pts <- st_sf(id = 1:5, geometry = st_sample(
        st_bbox(c(xmin = 0, xmax = 1, ymin = 0, ymax = 1)), 5), crs = 4326)
    path <- tempfile(fileext = ".parquet")
    on.exit(unlink(path), add = TRUE)
    writeGeoParquet(pts, path)
    ddb <- readGeoParquet(path)

    # st_collect() legitimately combines every row into ONE geometry (it
    # wraps DuckDB's own ST_Collect aggregate); st_as_sf() must not do that.
    sfobj <- st_as_sf(ddb)
    expect_s3_class(sfobj, "sf")
    expect_equal(nrow(sfobj), 5L)
    expect_true(all(as.character(st_geometry_type(sfobj)) == "POINT"))
    expect_identical(st_as_text(sfobj$geometry), st_as_text(pts$geometry))
    expect_identical(sfobj$id, pts$id)
    expect_true(st_crs(sfobj) == st_crs(4326))

    # No explicit CRS at write time still round-trips to a (spec-default)
    # CRS, not NA, and the geometry itself is unaffected either way.
    pts_nocrs <- st_sf(geometry = st_sample(
        st_bbox(c(xmin = 0, xmax = 1, ymin = 0, ymax = 1)), 3))
    path2 <- tempfile(fileext = ".parquet")
    on.exit(unlink(path2), add = TRUE)
    writeGeoParquet(pts_nocrs, path2)
    ddb2 <- readGeoParquet(path2)
    sfobj2 <- st_as_sf(ddb2)
    expect_false(is.na(st_crs(sfobj2)))
    expect_identical(st_as_text(sfobj2$geometry), st_as_text(pts_nocrs$geometry))

    expect_error(st_as_sf(DuckDBDataFrame(path)[, "id", drop = FALSE]),
                 "not a GEOMETRY column")
})

test_that("st_crs reads a lazy DuckDBTable/DuckDBColumn's CRS without collecting", {
    skip_if_not_installed("nanoparquet")
    pts <- st_sf(geometry = st_sfc(st_point(0:1), st_point(2:3), crs = 4326))
    path <- tempfile(fileext = ".parquet")
    on.exit(unlink(path), add = TRUE)
    writeGeoParquet(pts, path)
    ddb <- readGeoParquet(path)

    expect_true(st_crs(ddb) == st_crs(4326))
    expect_true(st_crs(ddb[["geometry"]]) == st_crs(4326))
    expect_error(st_crs(ddb, geom = "nonexistent"), "not a GEOMETRY column")
})

test_that("st_as_sf substitutes empty geometry for SQL NULL geometry cells", {
    conn <- DuckDBDataFrame::acquireDuckDBConn()
    ddb <- DuckDBDataFrame(dplyr::tbl(conn, dplyr::sql(
        "SELECT * FROM (VALUES (1, NULL::GEOMETRY), (2, ST_Point(1, 1)),
                                (3, NULL::GEOMETRY), (4, ST_Point(2, 2)))
         v(id, geometry)")))

    # as.data.frame() materializes a SQL-NULL geometry cell as a bare NULL
    # list element, which st_as_sfc.WKB() otherwise rejects outright.
    sfobj <- st_as_sf(ddb)
    expect_equal(nrow(sfobj), 4L)
    expect_identical(sfobj$id, 1:4)
    expect_identical(st_is_empty(sfobj$geometry), c(TRUE, FALSE, TRUE, FALSE))
    expect_equal(st_as_text(sfobj$geometry[[2L]]), "POINT (1 1)")
    expect_equal(st_as_text(sfobj$geometry[[4L]]), "POINT (2 2)")
})

test_that("st_as_sf converts every GEOMETRY-typed column, not just 'geom'", {
    conn <- DuckDBDataFrame::acquireDuckDBConn()
    ddb <- DuckDBDataFrame(dplyr::tbl(conn, dplyr::sql(
        "SELECT ST_Point(x, y) AS geometry,
                ST_Buffer(ST_Point(x, y), 1.0) AS buffer
         FROM (VALUES (0, 0), (1, 1)) v(x, y)")))

    sfobj <- st_as_sf(ddb)
    expect_identical(attr(sfobj, "sf_column"), "geometry")
    # The non-active geometry column must not survive as inert raw WKB
    # (previously true: sf::st_write() would silently write opaque bytes).
    expect_s3_class(sfobj$buffer, "sfc_POLYGON")
    expect_true(all(as.character(st_geometry_type(sfobj$buffer)) == "POLYGON"))
})

test_that("st_as_sf forwards ... to sf::st_as_sf", {
    skip_if_not_installed("nanoparquet")
    pts <- st_sf(id = 1:2, geometry = st_sfc(st_point(0:1), st_point(2:3)), crs = 4326)
    path <- tempfile(fileext = ".parquet")
    on.exit(unlink(path), add = TRUE)
    writeGeoParquet(pts, path)
    ddb <- readGeoParquet(path)

    sfobj <- st_as_sf(ddb, agr = "constant")
    expect_identical(unname(as.character(st_agr(sfobj)["id"])), "constant")
})
