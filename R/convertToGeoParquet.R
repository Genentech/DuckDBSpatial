### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Convert a spatial file to GeoParquet, and read one lazily
###
### GDAL reads GeoJSON, GeoPackage, Shapefile and the like through DuckDB's
### ST_Read(). It parses the whole file again for every query, with no column
### pruning and no filter pushdown, so these files are converted once to
### GeoParquet, which DuckDB reads in place.
###

#' Convert a spatial file to GeoParquet, and read one as a lazy table
#'
#' \code{convertToGeoParquet()} writes a spatial file that GDAL can read, such as
#' GeoJSON, GeoPackage or Shapefile, as a GeoParquet file. DuckDB streams the
#' conversion, so the data does not pass through R.
#' \code{readSpatialFile()} converts a file in the same way and returns the
#' GeoParquet file as a lazy \code{DuckDBDataFrame} through
#' \code{\link{readGeoParquet}()}.
#'
#' GDAL reads a file through DuckDB's \code{ST_Read()}, which parses the whole
#' file again for every query and cannot skip columns or rows. GeoParquet is
#' columnar and carries statistics, so a query on the converted file is fast. The
#' conversion costs about as much as one query on the original file.
#'
#' @section Coordinate reference system:
#' GDAL gives a GeoJSON file that has no coordinate reference system the CRS
#' \code{EPSG:4326}, since GeoJSON is defined in WGS84. Coordinates that are not
#' longitude and latitude, such as the pixel coordinates of cell outlines from
#' an imaging platform, are then labelled wrongly. Use \code{crs = NA} to record
#' the CRS as unknown, or give the CRS that the coordinates are in. Neither
#' reprojects the coordinates.
#'
#' With \code{crs = NA} the file records the CRS as unknown, an explicit
#' \code{null} in the GeoParquet metadata. A file without a CRS in its metadata
#' is read as \code{OGC:CRS84}, so an unknown CRS cannot be written by leaving
#' it out. Because DuckDB does not write that metadata itself in this case, the
#' source is read once more to find the bounding box and the geometry types.
#'
#' @param from The file to convert: a path that GDAL can open.
#' @param to The path of the GeoParquet file to write.
#' @param layer The name of the layer to read, for a source with several layers.
#'   \code{NULL} reads the first layer.
#' @param crs What to do with the coordinate reference system. \code{NULL}
#'   keeps the CRS that GDAL reports. \code{NA} records the CRS as unknown. An
#'   EPSG code (such as \code{3857}), a CRS string (such as
#'   \code{"EPSG:3857"} or \code{"OGC:CRS84"}) or an \code{sf} \code{crs} object
#'   assigns that CRS. See the section on the coordinate reference system.
#' @param geom The name of the geometry column to write. GDAL names it
#'   \code{geom}.
#' @param keep_fid If \code{TRUE}, keep the column \code{OGC_FID}, the 0-based
#'   index of the feature in the file. It is dropped by default.
#' @param open_options,allowed_drivers Character vectors passed on to
#'   \code{ST_Read()}: the GDAL open options of the driver, and the names of the
#'   drivers to try.
#' @param overwrite If \code{FALSE}, an existing \code{to} is an error.
#' @param conn A DuckDB DBI connection. When \code{NULL}, uses
#'   \code{\link[DuckDBDataFrame]{acquireDuckDBConn}()}.
#'
#' @return \code{convertToGeoParquet()} returns \code{to}, invisibly.
#'   \code{readSpatialFile()} returns a
#'   \link[DuckDBDataFrame:DuckDBDataFrame-class]{DuckDBDataFrame}.
#'
#' @seealso \code{\link{readGeoParquet}}, \code{\link{writeGeoParquet}}
#'
#' @examples
#' polys <- sf::st_sf(cell_id = c("c3", "c1", "c2"),
#'                    geometry = sf::st_sfc(sf::st_point(c(0, 0)),
#'                                          sf::st_point(c(1, 1)),
#'                                          sf::st_point(c(2, 2))),
#'                    crs = 4326)
#' geojson <- tempfile(fileext = ".geojson")
#' sf::st_write(polys, geojson, quiet = TRUE)
#'
#' parquet <- tempfile(fileext = ".parquet")
#' convertToGeoParquet(geojson, parquet)
#' readGeoParquet(parquet)
#'
#' ddb <- readSpatialFile(geojson, keycol = "cell_id")
#' rownames(ddb)
#'
#' @name convertToGeoParquet
#' @export
#' @importFrom DBI dbExecute dbGetQuery
#' @importFrom DuckDBDataFrame acquireDuckDBConn escapeSQLPath resolveParquetCompression
convertToGeoParquet <-
function(from, to, layer = NULL, crs = NULL, geom = "geometry",
         keep_fid = FALSE, open_options = NULL, allowed_drivers = NULL,
         overwrite = FALSE, conn = NULL)
{
    .check_string(from, "from")
    .check_string(to, "to")
    .check_string(geom, "geom")
    if (!is.null(layer))
        .check_string(layer, "layer")
    if (!isTRUE(keep_fid) && !isFALSE(keep_fid))
        stop("'keep_fid' must be TRUE or FALSE")
    if (!isTRUE(overwrite) && !isFALSE(overwrite))
        stop("'overwrite' must be TRUE or FALSE")
    if (!.is_gdal_uri(from) && !file.exists(from))
        stop("'", from, "' does not exist")
    if (file.exists(to) && !overwrite)
        stop("'", to, "' already exists; use 'overwrite = TRUE' to replace it")
    if (!dir.exists(dirname(to)))
        stop("the directory of '", to, "' does not exist")
    crs_mode <- .crs_mode(crs)

    if (is.null(conn))
        conn <- acquireDuckDBConn()
    enableGeoParquetConversion(conn)

    source <- .st_read_sql(from, layer, open_options, allowed_drivers)
    columns <- .st_read_columns(conn, source, from)
    drop <- c("geom", if (!keep_fid) "OGC_FID")
    kept <- setdiff(columns, drop)
    if (geom %in% kept)
        stop("'", from, "' has a column named '", geom, "'; give 'geom' a ",
             "different name for the geometry column")

    geom_sql <- switch(crs_mode,
                       keep = '"geom"',
                       unknown = '"geom"::GEOMETRY',
                       assign = sprintf('ST_SetCRS("geom", %s)', .sql_string(.crs_to_sql(crs))))
    select_sql <- sprintf("SELECT %s FROM %s",
                          paste(c(vapply(kept, .sql_identifier, ""),
                                  sprintf("%s AS %s", geom_sql, .sql_identifier(geom))),
                                collapse = ", "),
                          source)

    options <- .parquet_copy_options()
    if (crs_mode == "unknown") {
        geo <- .unknown_crs_geo_metadata(conn, source, geom)
        options <- c(options, sprintf("KV_METADATA {geo: %s}", .sql_string(geo)))
    }

    # write to a file next to the target and rename it, so that a failure does
    # not leave a partial file at 'to'
    part <- tempfile(pattern = paste0(basename(to), "."), tmpdir = dirname(to),
                     fileext = ".part")
    on.exit(unlink(part), add = TRUE)
    dbExecute(conn, sprintf("COPY (%s) TO '%s' (%s)", select_sql,
                            escapeSQLPath(part), paste(options, collapse = ", ")))
    if (file.exists(to))
        unlink(to)
    if (!file.rename(part, to))
        stop("could not move the converted file to '", to, "'")
    invisible(to)
}

#' @rdname convertToGeoParquet
#' @param path The file to read: a path that GDAL can open.
#' @param keycol The column to use as the key of the table, passed to
#'   \code{\link[DuckDBDataFrame]{DuckDBDataFrame}}. The default numbers the
#'   rows. \code{"OGC_FID"} keeps and uses the index of the feature in the file.
#' @param ... In \code{readSpatialFile()}, passed to
#'   \code{convertToGeoParquet()}.
#' @export
readSpatialFile <-
function(path, layer = NULL, crs = NULL, keycol = NULL, geom = "geometry",
         to = NULL, overwrite = FALSE, ...)
{
    if (is.null(to)) {
        to <- tempfile(pattern = "spatial_", fileext = ".parquet")
    } else {
        .check_string(to, "to")
    }
    convertToGeoParquet(path, to, layer = layer, crs = crs, geom = geom,
                        keep_fid = identical(keycol, "OGC_FID"),
                        overwrite = overwrite, ...)
    readGeoParquet(to, geom = geom, keycol = keycol)
}

.check_string <- function(x, what) {
    if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x))
        stop("'", what, "' must be a single non-empty string")
    invisible(x)
}

# a path that GDAL opens through a virtual file system or a URL, which is not a
# file of the local file system
.is_gdal_uri <- function(x) {
    grepl("^/vsi", x) || grepl("^[A-Za-z][A-Za-z0-9+.-]*://", x)
}

.sql_string <- function(x) {
    sprintf("'%s'", gsub("'", "''", x, fixed = TRUE))
}

.sql_identifier <- function(x) {
    sprintf('"%s"', gsub('"', '""', x, fixed = TRUE))
}

.json_string <- function(x) {
    paste0('"', gsub('(["\\\\])', "\\\\\\1", x), '"')
}

# keep the CRS that GDAL reports, record it as unknown, or assign one
.crs_mode <- function(crs) {
    if (is.null(crs))
        return("keep")
    if (inherits(crs, "crs"))
        return(if (is.na(crs)) "unknown" else "assign")
    if (length(crs) == 1L && is.na(crs))
        return("unknown")
    .crs_to_sql(crs)
    "assign"
}

# the text that ST_SetCRS() takes
.crs_to_sql <- function(crs) {
    if (inherits(crs, "crs"))
        crs <- crs$input
    if (is.numeric(crs) && length(crs) == 1L && !is.na(crs))
        crs <- sprintf("EPSG:%d", as.integer(crs))
    if (!is.character(crs) || length(crs) != 1L || is.na(crs) || !nzchar(trimws(crs)))
        stop("'crs' must be NULL, NA, an EPSG code, a CRS string or an sf 'crs' object")
    trimws(crs)
}

.st_read_sql <- function(from, layer, open_options, allowed_drivers) {
    args <- sprintf("'%s'", escapeSQLPath(from))
    if (!is.null(layer))
        args <- c(args, sprintf("layer := %s", .sql_string(layer)))
    if (length(open_options))
        args <- c(args, sprintf("open_options := [%s]",
                                paste(.sql_string(open_options), collapse = ", ")))
    if (length(allowed_drivers))
        args <- c(args, sprintf("allowed_drivers := [%s]",
                                paste(.sql_string(allowed_drivers), collapse = ", ")))
    sprintf("ST_Read(%s)", paste(args, collapse = ", "))
}

# the columns of the source, which also checks that GDAL can read it and that
# it has a geometry column
.st_read_columns <- function(conn, source, from) {
    columns <- tryCatch(
        dbGetQuery(conn, sprintf("SELECT column_name FROM (DESCRIBE SELECT * FROM %s)",
                                 source))[["column_name"]],
        error = function(e) {
            stop("could not read '", from, "' with GDAL: ",
                 sub("\n.*", "", conditionMessage(e)), call. = FALSE)
        })
    if (!"geom" %in% columns)
        stop("'", from, "' has no geometry column")
    columns
}

# DuckDB writes its own GeoParquet metadata, with the CRS in it, when the
# geometry has a CRS; the codec is the one that the rest of the suite writes
.parquet_copy_options <- function() {
    compression <- resolveParquetCompression()
    c("FORMAT PARQUET",
      sprintf("COMPRESSION %s", compression[["compression"]]),
      if (!is.null(compression[["compression_level"]]))
          sprintf("COMPRESSION_LEVEL %d", as.integer(compression[["compression_level"]])))
}

.geoparquet_types <- c(POINT = "Point", LINESTRING = "LineString",
                       POLYGON = "Polygon", MULTIPOINT = "MultiPoint",
                       MULTILINESTRING = "MultiLineString",
                       MULTIPOLYGON = "MultiPolygon",
                       GEOMETRYCOLLECTION = "GeometryCollection")

# The GeoParquet metadata of a geometry column whose CRS is unknown. DuckDB
# leaves the CRS out of the metadata it writes for a geometry without one, and
# a GeoParquet file without a CRS is read as OGC:CRS84, so the metadata is
# written here with an explicit null. The geometry types are left empty, which
# means unknown, when a type is not one of the plain 2D types.
.unknown_crs_geo_metadata <- function(conn, source, geom) {
    info <- dbGetQuery(conn, sprintf(paste0(
        "SELECT ST_XMin(e) AS xmin, ST_YMin(e) AS ymin, ST_XMax(e) AS xmax, ",
        "ST_YMax(e) AS ymax, t AS types, z AS has_z FROM (",
        "SELECT ST_Envelope_Agg(\"geom\") AS e, ",
        "list(DISTINCT ST_GeometryType(\"geom\")::VARCHAR) AS t, ",
        "bool_or(ST_HasZ(\"geom\")) AS z FROM %s WHERE \"geom\" IS NOT NULL)"),
        source))
    types <- unlist(info[["types"]])
    known <- length(types) > 0L && all(types %in% names(.geoparquet_types)) &&
        !isTRUE(info[["has_z"]])
    types_json <- if (known) {
        paste(.json_string(unname(.geoparquet_types[types])), collapse = ",")
    } else ""
    bbox <- unlist(info[c("xmin", "ymin", "xmax", "ymax")])
    bbox_json <- if (length(bbox) == 4L && all(is.finite(bbox))) {
        sprintf('"bbox":[%s],', paste(sprintf("%.17g", bbox), collapse = ","))
    } else ""
    sprintf(paste0('{"version":"1.0.0","primary_column":%s,"columns":{%s:',
                   '{"encoding":"WKB","geometry_types":[%s],%s"crs":null}}}'),
            .json_string(geom), .json_string(geom), types_json, bbox_json)
}
