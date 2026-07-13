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
    # OGC:CRS84 (WGS84 lon/lat). sf exposes WKT2; a genuinely unknown CRS is
    # left out (the WGS84 default then applies, as intended for unknown data).
    crs <- st_crs(sfc)
    if (!is.na(crs) && !is.null(crs$wkt)) {
        col_spec[["crs"]] <- crs$wkt
    }

    meta <- list(version = "1.0.0",
                 primary_column = geom,
                 columns = structure(list(col_spec), names = geom))
    jsonlite::toJSON(meta, auto_unbox = TRUE)
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
#' \dontrun{
#' library(sf)
#'
#' # Write a simple features object
#' nc <- st_read(system.file("shape/nc.shp", package="sf"))
#' writeGeoParquet(nc, "nc.parquet")
#'
#' # Write with custom geometry column name
#' pts <- st_sf(id = 1:3, geom = st_sfc(st_point(c(0,0)),
#'                                        st_point(c(1,1)),
#'                                        st_point(c(2,2))))
#' writeGeoParquet(pts, "points.parquet", geom = "geom")
#'
#' # Read back in DuckDB
#' library(DuckDBDataFrame)
#' ddb <- DuckDBDataFrame("nc.parquet")
#' ddb$geometry  # Native GEOMETRY column
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
#' @importFrom sf st_as_binary st_as_sfc st_geometry
writeGeoParquet <-
function(x, path, geom = "geometry", compression = "zstd",
         options = nanoparquet::parquet_options(compression_level = 3L), ...)
{
    if (!requireNamespace("nanoparquet", quietly = TRUE))
        stop("package 'nanoparquet' is required for writeGeoParquet(); ",
             "install with install.packages('nanoparquet')")

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

    meta <- .buildGeoParquetMetadata(geom, sfc)
    nanoparquet::write_parquet(df, file = path, metadata = c(geo = meta),
                               compression = compression, options = options, ...)

    invisible(NULL)
}
