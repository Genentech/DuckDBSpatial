# Tests the basic functions of a DuckDBColumn.
# library(testthat); library(DuckDBSpatial); source("setup.R"); source("test-DuckDBColumn.R")

test_that("Spatial coercion methods work as expected for a DuckDBColumn", {
    df <- DuckDBDataFrame(spatial_path)
    df <- df[which(!is.na(spatial_wkt)),]

    x <- df[["geometry"]]
    type <- df[["type"]]
    sfc <- st_as_sfc(spatial_wkt[!is.na(spatial_wkt)])

    wkt <- st_as_text(x)
    checkDuckDBColumn(st_as_text(st_as_sfc(wkt)), st_as_text(sfc))
    checkDuckDBColumn(st_as_binary(st_as_sfc(wkt), hex = TRUE), st_as_binary(sfc, hex = TRUE))
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
