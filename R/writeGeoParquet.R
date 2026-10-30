### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### GeoParquet Helper Functions
###

.geometryTypeToGeoParquet <- function(types) {
    # Map sf geometry type names to GeoParquet 1.0 type names
    map <- c(POINT = "Point",
             LINESTRING = "LineString",
             POLYGON = "Polygon",
             MULTIPOINT = "MultiPoint",
             MULTILINESTRING = "MultiLineString",
             MULTIPOLYGON = "MultiPolygon",
             GEOMETRYCOLLECTION = "GeometryCollection")
    u <- unique(toupper(types))
    u <- u[nzchar(u)]
    if (length(u) == 0L) {
        "GeometryCollection"
    } else {
        out <- map[u]
        out[is.na(out)] <- "GeometryCollection"
        unique(as.character(out))
    }
}

#' @importFrom sf st_bbox st_crs st_geometry_type
#' @importFrom wk wk_crs_projjson
.buildGeoParquetMetadata <- function(geom, sfc) {
    if (!requireNamespace("jsonlite", quietly = TRUE))
        stop("package 'jsonlite' is required for writeGeoParquet(); ",
             "install with install.packages('jsonlite')")

    types <- as.character(st_geometry_type(sfc))
    geometry_types <- .geometryTypeToGeoParquet(types)

    col_spec <- list(encoding = "WKB",
                     geometry_types = as.list(geometry_types))

    # bbox is OPTIONAL in the GeoParquet spec and should be OMITTED when the
    # extent is unknown (empty / non-finite).
    bbox <- as.numeric(st_bbox(sfc))
    if (all(is.finite(bbox))) {
        col_spec[["bbox"]] <- as.list(bbox)
    }

    # Emit the CRS so a reader does not fall back to the spec default of
    # OGC:CRS84 (WGS84 lon/lat). The GeoParquet 1.0.0 spec requires "crs" to
    # be a PROJJSON object, not a WKT string, so it's parsed back into a list
    # here for jsonlite::toJSON() to re-embed as a nested JSON object rather
    # than a JSON-encoded string. A genuinely unknown CRS, or one PROJJSON
    # can't represent, is left out (the WGS84 default then applies).
    crs <- st_crs(sfc)
    if (!is.na(crs)) {
        projjson <- tryCatch(wk_crs_projjson(crs), error = function(e) NULL)
        if (!is.null(projjson)) {
            col_spec[["crs"]] <- jsonlite::fromJSON(projjson, simplifyVector = FALSE)
        }
    }

    meta <- list(version = "1.0.0",
                 primary_column = geom,
                 columns = structure(list(col_spec), names = geom))
    # digits = NA: jsonlite::toJSON() defaults to 4 significant digits, which
    # silently truncates the embedded CRS's ellipsoid parameters (e.g. WGS84's
    # inverse_flattening 298.257223563 -> 298.2572) and bbox coordinates.
    jsonlite::toJSON(meta, auto_unbox = TRUE, digits = NA)
}

#' @importFrom sf st_crs
.mergeGeoParquetMetadata <- function(path, geom, sfc) {
    kv <- nanoparquet::read_parquet_metadata(path)$file_meta_data$key_value_metadata[[1L]]
    old_geo <- kv$value[kv$key == "geo"]
    if (length(old_geo) == 0L)
        stop("'", path, "' has no GeoParquet 'geo' metadata; write it first ",
             "with 'append = FALSE'")
    old_meta <- jsonlite::fromJSON(old_geo, simplifyVector = FALSE)
    if (!identical(old_meta[["primary_column"]], geom))
        stop("'geom' ('", geom, "') does not match the existing file's ",
             "primary geometry column ('", old_meta[["primary_column"]], "')")
    old_spec <- old_meta[["columns"]][[geom]]
    if (is.null(old_spec))
        stop("'", path, "' has no GeoParquet metadata for column '", geom, "'")

    old_crs <- if (is.null(old_spec[["crs"]])) {
        st_crs("OGC:CRS84")
    } else {
        st_crs(as.character(jsonlite::toJSON(old_spec[["crs"]], auto_unbox = TRUE,
                                             digits = NA)))
    }
    new_crs <- st_crs(sfc)
    if (!is.na(new_crs) && (old_crs != new_crs))
        stop("CRS of 'x' does not match the existing file's CRS; ",
             "reproject 'x' to match before appending")

    old_types <- unlist(old_spec[["geometry_types"]])
    new_types <- .geometryTypeToGeoParquet(as.character(st_geometry_type(sfc)))
    old_spec[["geometry_types"]] <- as.list(unique(c(old_types, new_types)))

    old_bbox <- old_spec[["bbox"]]
    new_bbox <- as.numeric(st_bbox(sfc))
    if (!is.null(old_bbox) && all(is.finite(new_bbox))) {
        old_bbox <- as.numeric(unlist(old_bbox))
        old_spec[["bbox"]] <- as.list(c(pmin(old_bbox[1:2], new_bbox[1:2]),
                                         pmax(old_bbox[3:4], new_bbox[3:4])))
    } else {
        # Unknown old extent (never recorded) or non-finite new extent: can't
        # reconstruct a true combined bbox, so omit it rather than report one
        # that silently excludes rows (bbox is optional in the spec).
        old_spec[["bbox"]] <- NULL
    }

    old_meta[["columns"]][[geom]] <- old_spec
    jsonlite::toJSON(old_meta, auto_unbox = TRUE, digits = NA)
}

#' Write spatial data as GeoParquet
#'
#' @description
#' Writes \code{sf} objects or data frames with geometry columns as GeoParquet
#' files conforming to the GeoParquet 1.0.0 specification. The geometry column
#' is encoded as Well-Known Binary (WKB) and appropriate metadata is added to
#' ensure compatibility with DuckDB's spatial extension, GeoPandas, and other
#' GeoParquet readers.
#'
#' @param x An \code{sf} object or a data frame with a geometry column
#'   (either \code{sfc} or a list of raw WKB vectors).
#' @param path Character string specifying the output file path. Should end
#'   in \code{.parquet}.
#' @param geom Character string specifying the name of the geometry column.
#'   Defaults to \code{"geometry"}. For \code{sf} objects, this is automatically
#'   detected if not provided.
#' @param compression Character string specifying the compression algorithm.
#'   Defaults to \code{"zstd"}. Other options include \code{"snappy"},
#'   \code{"gzip"}, \code{"brotli"}, or \code{"uncompressed"}.
#' @param options Optional list of parquet options created by
#'   \code{nanoparquet::parquet_options()}. Defaults to compression level 3.
#' @param append Logical; if \code{TRUE}, append \code{x} as new row groups
#'   to an existing GeoParquet file at \code{path} via
#'   \code{\link[nanoparquet]{append_parquet}}, instead of overwriting it.
#'   \code{path} must already exist and carry GeoParquet metadata (i.e. have
#'   been written by \code{writeGeoParquet()}, possibly in an earlier
#'   \code{append = TRUE} call); the \code{geo} metadata (geometry types,
#'   bounding box, CRS) is then updated in place to cover the combined file.
#'   The CRS of \code{x} must match the existing file's CRS; appending to a
#'   multi-part/hive-partitioned layout is not supported. Like
#'   \code{append_parquet()} itself, this is \strong{not atomic}: an
#'   interrupted append can leave \code{path} corrupt.
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns \code{NULL}. Called for its side effect of writing
#'   a GeoParquet file to disk.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Converts the geometry column to Well-Known Binary (WKB) format
#'   \item Generates GeoParquet 1.0.0 metadata including:
#'     \itemize{
#'       \item Geometry type(s) (Point, LineString, Polygon, etc.)
#'       \item Bounding box (xmin, ymin, xmax, ymax)
#'       \item Encoding format (WKB)
#'       \item Primary geometry column name
#'     }
#'   \item Writes the data to a Parquet file using \pkg{nanoparquet}
#' }
#'
#' The resulting files can be read by:
#' \itemize{
#'   \item DuckDB: \code{SELECT ST_AsText(geometry) FROM 'file.parquet'}
#'   \item GeoPandas: \code{gpd.read_parquet('file.parquet')}
#'   \item GDAL/OGR: \code{ogr2ogr -f GeoJSON output.json file.parquet}
#'   \item \pkg{sf}: \code{sf::st_read('file.parquet')}
#' }
#'
#' @examples
#' library(sf)
#' nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#'
#' \donttest{
#' if (requireNamespace("nanoparquet", quietly = TRUE)) {
#'     # Write a simple features object
#'     nc_path <- tempfile(fileext = ".parquet")
#'     writeGeoParquet(nc, nc_path)
#'
#'     # Write with a custom geometry column name
#'     pts <- st_sf(id = 1:3, geom = st_sfc(st_point(c(0, 0)),
#'                                          st_point(c(1, 1)),
#'                                          st_point(c(2, 2))))
#'     pts_path <- tempfile(fileext = ".parquet")
#'     writeGeoParquet(pts, pts_path, geom = "geom")
#'
#'     # Append more features to an existing GeoParquet file
#'     more_pts <- st_sf(id = 4:5, geom = st_sfc(st_point(c(3, 3)),
#'                                                st_point(c(4, 4))))
#'     writeGeoParquet(more_pts, pts_path, geom = "geom", append = TRUE)
#'
#'     # Read back in DuckDB
#'     ddb <- DuckDBDataFrame::DuckDBDataFrame(nc_path)
#'     ddb$geometry  # Native GEOMETRY column
#'
#'     unlink(c(nc_path, pts_path))
#' }
#' }
#'
#' @seealso
#' \itemize{
#'   \item \url{https://geoparquet.org/} - GeoParquet specification
#'   \item \code{\link[nanoparquet]{write_parquet}} - Underlying Parquet writer
#'   \item \code{\link[sf]{st_write}} - Alternative sf-based writer
#' }
#'
#' @export
#' @importFrom S4Vectors isTRUEorFALSE
#' @importFrom sf st_as_binary st_as_sfc st_geometry
writeGeoParquet <-
function(x, path, geom = "geometry", compression = "zstd",
         options = nanoparquet::parquet_options(compression_level = 3L),
         append = FALSE, ...)
{
    if (!requireNamespace("nanoparquet", quietly = TRUE))
        stop("package 'nanoparquet' is required for writeGeoParquet(); ",
             "install with install.packages('nanoparquet')")
    if (!isTRUEorFALSE(append))
        stop("'append' must be TRUE or FALSE")
    if (isTRUE(append) && !file.exists(path))
        stop("'append = TRUE' but '", path, "' does not exist; ",
             "write it first with 'append = FALSE'")

    if (inherits(x, "sf")) {
        df <- as.data.frame(x, optional = TRUE)
        sfc <- st_geometry(x)
        if (!geom %in% names(df)) {
            gcol <- attr(x, "sf_column")
            geom <- if (is.null(gcol)) "geometry" else gcol
        }
        df[[geom]] <- I(st_as_binary(sfc, hex = FALSE))
    } else {
        df <- as.data.frame(x, optional = TRUE)
        if (!geom %in% names(df))
            stop("geometry column '", geom, "' not found in 'x'")
        col <- df[[geom]]
        if (inherits(col, "sfc")) {
            sfc <- col
            df[[geom]] <- I(st_as_binary(sfc, hex = FALSE))
        } else if (is.list(col) && all(vapply(col, function(r) is.null(r) || is.raw(r), NA))) {
            # Already WKB (list of raw)
            sfc <- tryCatch(st_as_sfc(structure(col, class = "WKB")),
                            error = function(e) NULL)
            if (is.null(sfc))
                stop("cannot infer geometry types from WKB column; pass sf object")
        } else {
            stop("geometry column must be sfc or list of raw (WKB)")
        }
    }

    if (isTRUE(append)) {
        if (!requireNamespace("blob", quietly = TRUE))
            stop("package 'blob' is required for 'writeGeoParquet(..., ",
                 "append = TRUE)'; install with install.packages('blob')")
        meta <- .mergeGeoParquetMetadata(path, geom, sfc)
        df[[geom]] <- blob::as_blob(unclass(df[[geom]]))
        nanoparquet::append_parquet(df, file = path, compression = compression,
                                    options = options, ...)
        nanoparquet::edit_parquet_metadata(path, c(geo = meta))
    } else {
        meta <- .buildGeoParquetMetadata(geom, sfc)
        nanoparquet::write_parquet(df, file = path, metadata = c(geo = meta),
                                   compression = compression, options = options, ...)
    }

    invisible(NULL)
}
