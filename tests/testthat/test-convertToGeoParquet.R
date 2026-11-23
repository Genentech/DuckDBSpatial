# convertToGeoParquet() and readSpatialFile(): GDAL files to GeoParquet.

.cells_sf <- function(crs = 3857) {
    sf::st_sf(cell_id = c("c3", "c1", "c2"), area = c(3.5, 1.5, 2.5),
              geometry = sf::st_sfc(
                  sf::st_polygon(list(rbind(c(0, 0), c(10, 0), c(10, 10), c(0, 0)))),
                  sf::st_polygon(list(rbind(c(20, 20), c(30, 20), c(30, 30), c(20, 20)))),
                  sf::st_polygon(list(rbind(c(50, 50), c(60, 50), c(60, 60), c(50, 50))))),
              crs = crs)
}

.write_geojson <- function(x) {
    path <- tempfile(fileext = ".geojson")
    suppressWarnings(sf::st_write(x, path, quiet = TRUE))
    path
}

.geo_metadata <- function(path) {
    arrow::read_parquet(path, as_data_frame = FALSE)$schema$metadata[["geo"]]
}

.geometry_type <- function(path) {
    conn <- DuckDBDataFrame::acquireDuckDBConn()
    DBI::dbGetQuery(conn, sprintf("SELECT typeof(geometry) AS t FROM read_parquet('%s') LIMIT 1",
                                  path))[["t"]]
}

test_that("convertToGeoParquet writes a GeoParquet file that keeps the attributes, geometries and CRS", {
    cells <- .cells_sf()
    from <- .write_geojson(cells)
    to <- tempfile(fileext = ".parquet")
    on.exit(unlink(c(from, to)), add = TRUE)

    expect_identical(convertToGeoParquet(from, to), to)
    expect_true(file.exists(to))
    expect_identical(names(arrow::read_parquet(to, as_data_frame = FALSE)$schema),
                     c("cell_id", "area", "geometry"))          # OGC_FID is dropped
    expect_match(.geo_metadata(to), "\"encoding\":\"WKB\"", fixed = TRUE)

    back <- readGeoParquet(to)
    expect_s4_class(back, "DuckDBDataFrame")
    expect_identical(nrow(back), 3L)
    expect_identical(.geometry_type(to), "GEOMETRY('EPSG:3857')")
    got <- sf::st_as_sf(back)
    expect_identical(as.character(got$cell_id), cells$cell_id)
    expect_equal(got$area, cells$area)
    expect_identical(sf::st_as_text(sf::st_geometry(got)),
                     sf::st_as_text(sf::st_geometry(cells)))
    expect_identical(sf::st_crs(got)$epsg, 3857L)
})

test_that("the CRS can be kept, assigned or recorded as unknown", {
    from <- .write_geojson(.cells_sf(crs = 3857))
    on.exit(unlink(from), add = TRUE)
    out <- function(...) {
        to <- tempfile(fileext = ".parquet")
        convertToGeoParquet(from, to, ...)
        to
    }

    # kept: whatever GDAL reports for the file
    kept <- out()
    expect_match(.geometry_type(kept), "^GEOMETRY\\('")
    unlink(kept)

    # assigned: the coordinates are not reprojected
    for (crs in list(4326, "EPSG:4326", sf::st_crs(4326))) {
        to <- out(crs = crs)
        expect_identical(.geometry_type(to), "GEOMETRY('EPSG:4326')")
        conn <- DuckDBDataFrame::acquireDuckDBConn()
        xmax <- DBI::dbGetQuery(conn, sprintf("SELECT max(ST_XMax(geometry)) AS x FROM read_parquet('%s')", to))$x
        expect_equal(xmax, 60)
        unlink(to)
    }

    # unknown: an explicit null, which is not the same as leaving the CRS out, as
    # a GeoParquet file without a CRS is read as OGC:CRS84
    for (crs in list(NA, NA_integer_, sf::st_crs(NA))) {
        to <- out(crs = crs)
        md <- .geo_metadata(to)
        expect_match(md, "\"crs\":null", fixed = TRUE)
        expect_match(md, "\"geometry_types\":[\"Polygon\"]", fixed = TRUE)
        expect_match(md, "\"bbox\":[0,0,60,60]", fixed = TRUE)
        expect_identical(.geometry_type(to), "GEOMETRY")
        unlink(to)
    }

    expect_error(out(crs = "   "), "'crs'")
    expect_error(out(crs = c(4326, 3857)), "'crs'")
    expect_error(out(crs = TRUE), "'crs'")
})

test_that("an unknown CRS records the geometry types only when they are plain 2D types", {
    mixed <- sf::st_sf(id = 1:3, geometry = sf::st_sfc(
        sf::st_point(c(1, 2)), sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 0)))),
        sf::st_multipoint(rbind(c(1, 1), c(2, 2)))))
    from <- .write_geojson(mixed)
    to <- tempfile(fileext = ".parquet")
    on.exit(unlink(c(from, to)), add = TRUE)
    convertToGeoParquet(from, to, crs = NA)
    md <- .geo_metadata(to)
    for (type in c("Point", "Polygon", "MultiPoint"))
        expect_match(md, sprintf("\"%s\"", type), fixed = TRUE)

    # a Z geometry: the types are left empty, which means unknown
    withz <- sf::st_sf(id = 1:2, geometry = sf::st_sfc(
        sf::st_point(c(1, 2, 3)), sf::st_point(c(4, 5, 6))))
    from_z <- .write_geojson(withz)
    on.exit(unlink(from_z), add = TRUE)
    to_z <- tempfile(fileext = ".parquet")
    on.exit(unlink(to_z), add = TRUE)
    convertToGeoParquet(from_z, to_z, crs = NA)
    expect_match(.geo_metadata(to_z), "\"geometry_types\":[]", fixed = TRUE)
})

test_that("a layer of a source with several layers can be chosen", {
    cells <- .cells_sf()
    gpkg <- tempfile(fileext = ".gpkg")
    on.exit(unlink(gpkg), add = TRUE)
    suppressWarnings({
        sf::st_write(cells, gpkg, layer = "cells", quiet = TRUE)
        sf::st_write(cells[1:2, ], gpkg, layer = "nuclei", quiet = TRUE)
    })
    rows <- function(layer) {
        to <- tempfile(fileext = ".parquet")
        on.exit(unlink(to))
        convertToGeoParquet(gpkg, to, layer = layer)
        nrow(readGeoParquet(to))
    }
    expect_identical(rows("cells"), 3L)
    expect_identical(rows("nuclei"), 2L)
    expect_error(rows("no such layer"), "could not read")
})

test_that("the geometry column can be named, and a clash is an error", {
    from <- .write_geojson(.cells_sf())
    on.exit(unlink(from), add = TRUE)
    to <- tempfile(fileext = ".parquet")
    on.exit(unlink(to), add = TRUE)
    convertToGeoParquet(from, to, geom = "shape")
    expect_identical(names(arrow::read_parquet(to, as_data_frame = FALSE)$schema),
                     c("cell_id", "area", "shape"))
    expect_match(.geo_metadata(to), "\"primary_column\":\"shape\"", fixed = TRUE)
    expect_error(convertToGeoParquet(from, tempfile(fileext = ".parquet"), geom = "area"),
                 "a column named 'area'")
})

test_that("the index of the feature in the file is kept only on request", {
    from <- .write_geojson(.cells_sf())
    on.exit(unlink(from), add = TRUE)
    to <- tempfile(fileext = ".parquet")
    on.exit(unlink(to), add = TRUE)
    convertToGeoParquet(from, to, keep_fid = TRUE)
    expect_true("OGC_FID" %in% names(arrow::read_parquet(to, as_data_frame = FALSE)$schema))
})

test_that("convertToGeoParquet checks its arguments and does not leave a partial file", {
    from <- .write_geojson(.cells_sf())
    to <- tempfile(fileext = ".parquet")
    on.exit(unlink(c(from, to)), add = TRUE)

    expect_error(convertToGeoParquet(tempfile(fileext = ".geojson"), to), "does not exist")
    expect_error(convertToGeoParquet(c(from, from), to), "'from'")
    expect_error(convertToGeoParquet(from, NA_character_), "'to'")
    expect_error(convertToGeoParquet(from, file.path(tempdir(), "no", "dir", "x.parquet")),
                 "directory")
    expect_error(convertToGeoParquet(from, to, keep_fid = NA), "'keep_fid'")
    expect_error(convertToGeoParquet(from, to, overwrite = "yes"), "'overwrite'")

    # an existing target is an error unless it may be replaced
    convertToGeoParquet(from, to)
    expect_error(convertToGeoParquet(from, to), "already exists")
    expect_silent(convertToGeoParquet(from, to, overwrite = TRUE))

    # a file that GDAL cannot read fails without writing the target or a part file
    junk <- tempfile(fileext = ".geojson")
    writeLines("this is not a spatial file", junk)
    dest <- tempfile(fileext = ".parquet")
    before <- list.files(dirname(dest))
    expect_error(convertToGeoParquet(junk, dest), "could not read")
    expect_false(file.exists(dest))
    expect_identical(setdiff(list.files(dirname(dest)), c(before, basename(junk))), character())
    unlink(junk)
})

test_that("readSpatialFile returns a lazy table of the converted file", {
    cells <- .cells_sf()
    from <- .write_geojson(cells)
    on.exit(unlink(from), add = TRUE)

    # rows are numbered unless a key is given
    ddb <- readSpatialFile(from)
    expect_s4_class(ddb, "DuckDBDataFrame")
    expect_identical(colnames(ddb), c("cell_id", "area", "geometry"))
    expect_identical(nrow(ddb), 3L)
    expect_identical(as.integer(rownames(ddb)), 1:3)

    # a named key, in the stored order of the keys
    keyed <- readSpatialFile(from, keycol = "cell_id")
    expect_identical(rownames(keyed), c("c1", "c2", "c3"))

    # the index of the feature in the file as the key
    fid <- readSpatialFile(from, keycol = "OGC_FID")
    expect_identical(as.integer(rownames(fid)), 0:2)

    # crs is passed on
    unknown <- readSpatialFile(from, crs = NA)
    expect_identical(sf::st_crs(sf::st_as_sf(unknown)), sf::NA_crs_)
})

test_that("readSpatialFile keeps the converted file when it is given a path", {
    from <- .write_geojson(.cells_sf())
    to <- tempfile(fileext = ".parquet")
    on.exit(unlink(c(from, to)), add = TRUE)

    ddb <- readSpatialFile(from, to = to)
    expect_true(file.exists(to))
    expect_identical(nrow(readGeoParquet(to)), 3L)
    expect_identical(nrow(ddb), 3L)
    expect_error(readSpatialFile(from, to = to), "already exists")
    expect_identical(nrow(readSpatialFile(from, to = to, overwrite = TRUE)), 3L)
})
