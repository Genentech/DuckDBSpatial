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
#' @param ... Passed to \code{\link[DuckDBDataFrame]{DuckDBDataFrame}}.
#'
#' @return A \link[DuckDBDataFrame:DuckDBDataFrame-class]{DuckDBDataFrame}.
#'
#' @export
#' @importFrom DuckDBDataFrame DuckDBDataFrame
readGeoParquet <- function(path, ...) {
    enableGeoParquetConversion()
    DuckDBDataFrame(path, ...)
}
