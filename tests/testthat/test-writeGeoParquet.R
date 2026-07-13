test_that("writeGeoParquet requires nanoparquet", {
    ## When nanoparquet is installed we skip (cannot test the error)
    skip_if(requireNamespace("nanoparquet", quietly = TRUE), "nanoparquet is installed")
    expect_error(
        DuckDBSpatial::writeGeoParquet(sf::st_sf(geometry = sf::st_sfc(sf::st_point(0:1))), tempfile(fileext = ".parquet")),
        "nanoparquet"
    )
})

test_that("writeGeoParquet round-trip for POINT", {
    skip_if_not_installed("nanoparquet")
    library(sf)

    pts <- st_sfc(st_point(0:1), st_point(2:3), st_point(c(1.5, 2.5)))
    sf_pt <- st_sf(id = 1:3, geometry = pts)

    path <- tempfile(fileext = ".parquet")
    on.exit(unlink(path))

    DuckDBSpatial::writeGeoParquet(sf_pt, path)

    ## Metadata present
    mtd <- nanoparquet::read_parquet_metadata(path)
    kvm <- mtd$file_meta_data$key_value_metadata[[1]]
    expect_true("geo" %in% kvm$key)
    geo <- jsonlite::fromJSON(kvm$value[kvm$key == "geo"])
    expect_equal(geo$version, "1.0.0")
    expect_equal(geo$primary_column, "geometry")
    expect_true("Point" %in% unlist(geo$columns$geometry$geometry_types))

    ## Round-trip via nanoparquet read
    df <- nanoparquet::read_parquet(path)
    wkb <- df$geometry
    sfc_out <- st_as_sfc(structure(wkb, class = "WKB"))
    expect_equal(st_coordinates(sfc_out), st_coordinates(pts), check.attributes = FALSE)
})

test_that("writeGeoParquet records the CRS and omits an unknown bbox", {
    skip_if_not_installed("nanoparquet")
    library(sf)

    # Projected CRS (UTM 33N): the geo metadata must carry a `crs` so a reader
    # does not fall back to the WGS84 default and mislabel the coordinates.
    pts <- st_sf(id = 1:2, geometry = st_sfc(
        st_point(c(500000, 4649776)), st_point(c(500100, 4649876)), crs = 32633))
    path <- tempfile(fileext = ".parquet")
    on.exit(unlink(path))
    DuckDBSpatial::writeGeoParquet(pts, path)

    kvm <- nanoparquet::read_parquet_metadata(path)$file_meta_data$key_value_metadata[[1]]
    geo <- jsonlite::fromJSON(kvm$value[kvm$key == "geo"])
    cs <- geo$columns$geometry
    expect_false(is.null(cs$crs))
    expect_true(grepl("UTM zone 33N", cs$crs))          # WKT2 of EPSG:32633
    expect_equal(unlist(cs$bbox), c(500000, 4649776, 500100, 4649876))

    # An empty geometry has a non-finite extent → bbox must be OMITTED (not
    # [0,0,0,0], which would falsely claim a real extent at the origin).
    emp <- st_sf(id = 1L, geometry = st_sfc(st_geometrycollection(), crs = 4326))
    path2 <- tempfile(fileext = ".parquet")
    on.exit(unlink(path2), add = TRUE)
    DuckDBSpatial::writeGeoParquet(emp, path2)
    kvm2 <- nanoparquet::read_parquet_metadata(path2)$file_meta_data$key_value_metadata[[1]]
    geo2 <- jsonlite::fromJSON(kvm2$value[kvm2$key == "geo"])
    expect_null(geo2$columns$geometry$bbox)
})

test_that("writeGeoParquet round-trip for POLYGON", {
    skip_if_not_installed("nanoparquet")
    library(sf)

    poly <- st_sfc(st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 0)))))
    sf_poly <- st_sf(id = 1L, geometry = poly)

    path <- tempfile(fileext = ".parquet")
    on.exit(unlink(path))

    DuckDBSpatial::writeGeoParquet(sf_poly, path)

    df <- nanoparquet::read_parquet(path)
    sfc_out <- st_as_sfc(structure(df$geometry, class = "WKB"))
    expect_equal(st_geometry_type(sfc_out), st_geometry_type(poly))
    expect_equal(st_coordinates(sfc_out), st_coordinates(poly), check.attributes = FALSE)
})

test_that("writeGeoParquet DuckDB reads as GEOMETRY", {
    skip_if_not_installed("nanoparquet")
    library(sf)

    pts <- st_sfc(st_point(0:1), st_point(2:3))
    sf_pt <- st_sf(id = 1:2, geometry = pts)

    path <- tempfile(fileext = ".parquet")
    on.exit(unlink(path))
    DuckDBSpatial::writeGeoParquet(sf_pt, path)

    ## DuckDB with spatial + enable_geoparquet_conversion should read geometry as GEOMETRY
    conn <- DBI::dbConnect(duckdb::duckdb())
    on.exit(DBI::dbDisconnect(conn, shutdown = TRUE), add = TRUE)
    ## Spatial extension required for GEOMETRY type and GeoParquet conversion
    tryCatch(
        DBI::dbExecute(conn, "LOAD spatial"),
        error = function(e) skip("spatial extension not available")
    )
    DBI::dbExecute(conn, "SET enable_geoparquet_conversion = true")

    schema <- DBI::dbGetQuery(conn, paste0("DESCRIBE SELECT * FROM read_parquet('", path, "')"))
    geom_row <- schema[schema$column_name == "geometry", ]
    expect_true(nrow(geom_row) > 0)
    expect_true(grepl("^GEOMETRY", geom_row$column_type))
})

test_that("writeGeoParquet with DataFrame and WKB column errors without sfc", {
    skip_if_not_installed("nanoparquet")
    ## DataFrame with list of raw but no way to infer types - should error
    df <- S4Vectors::DataFrame(
        id = 1:2,
        geometry = I(list(charToRaw("invalid_wkb"), charToRaw("also_invalid")))
    )
    path <- tempfile(fileext = ".parquet")
    on.exit(unlink(path))
    expect_error(
        DuckDBSpatial::writeGeoParquet(df, path),
        "cannot infer geometry types"
    )
})
