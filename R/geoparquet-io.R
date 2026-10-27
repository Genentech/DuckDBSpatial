### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### GeoParquet connection setup and lazy read
###

#' Enable DuckDB GeoParquet conversion on a connection
#'
#' Sets \code{enable_geoparquet_conversion = true} so geometry columns in
#' Parquet files are read as native \code{GEOMETRY} types.
#'
#' @param conn A DuckDB DBI connection. When \code{NULL}, uses
#'   \code{\link[DuckDBDataFrame]{acquireDuckDBConn}()}.
#'
#' @return The connection, invisibly.
#'
#' @examples
#' enableGeoParquetConversion()
#'
#' @export
#' @importFrom DBI dbExecute
#' @importFrom DuckDBDataFrame acquireDuckDBConn
enableGeoParquetConversion <- function(conn = NULL) {
    if (is.null(conn))
        conn <- acquireDuckDBConn()
    tryCatch(
        dbExecute(conn, "SET enable_geoparquet_conversion = true"),
        error = function(e) invisible(NULL)
    )
    invisible(conn)
}

#' Read a GeoParquet file as a lazy \code{DuckDBDataFrame}
#'
#' @param path Path to a GeoParquet file or directory.
#' @param geom Geometry column name to check for after reading; warns if
#'   \code{path} has no such \code{GEOMETRY}-typed column (see
#'   \code{\link{hasGeometry}}), since that usually means \code{path}
#'   isn't actually a GeoParquet file. The read itself still succeeds and
#'   returns the (non-spatial) data either way.
#' @param ... Passed to \code{\link[DuckDBDataFrame]{DuckDBDataFrame}}.
#'
#' @return A \link[DuckDBDataFrame:DuckDBDataFrame-class]{DuckDBDataFrame}.
#'
#' @examples
#' spatial_path <- system.file("extdata", "spatial", package = "DuckDBSpatial")
#' ddb <- readGeoParquet(spatial_path)
#' nrow(ddb)
#' colnames(ddb)
#'
#' @export
#' @importFrom DuckDBDataFrame DuckDBDataFrame
readGeoParquet <- function(path, geom = "geometry", ...) {
    enableGeoParquetConversion()
    result <- DuckDBDataFrame(path, ...)
    if (!hasGeometry(result, geom)) {
        warning("no '", geom, "' GEOMETRY column found in '", path, "'; ",
                "this may not be a GeoParquet file")
    }
    result
}
