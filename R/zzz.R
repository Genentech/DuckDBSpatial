# Best-effort install/load of the DuckDB `spatial` extension on the shared
# DuckDBDataFrame connection when DuckDBSpatial is loaded. Every DuckDBSpatial
# operation compiles to `ST_*` SQL resolved against this connection at collection
# time, so the extension must be present; DuckDB's autoload does not reliably
# install it (e.g. in a fresh R CMD check environment).
#
# This runs QUIETLY: where the extension cannot be obtained (offline / restricted
# network / TLS interception) it is skipped without a warning or message — a
# package's .onLoad must not emit output, and spatial operations then fail lazily
# with DuckDB's own "install the spatial extension" guidance only when actually
# used. To pre-seed in a restricted environment, place a matching
# `spatial.duckdb_extension` under `DUCKDB_EXTENSION_DIRECTORY`, or point
# `BIOCDUCKDB_EXTENSION_REPOSITORY` at a reachable mirror.

# DuckDB 2.0 refuses ST_Transform and the spheroid functions until
# 'geometry_always_xy' is set, because the axis order of a CRS (northing first
# for some) would otherwise decide what a coordinate pair means. Coordinates
# here are always (x, y), which is what the setting says and what DuckDB 2.1
# makes the default, so it is set once on the shared connection.
#' @importFrom DBI dbExecute dbGetQuery
.setAlwaysXY <- function(conn) {
    known <- dbGetQuery(conn, paste("SELECT 1 FROM duckdb_settings()",
                                    "WHERE name = 'geometry_always_xy'"))
    if (nrow(known))
        dbExecute(conn, "SET geometry_always_xy = true")
    invisible(NULL)
}

#' @importFrom DuckDBDataFrame acquireDuckDBConn loadExtension
.onLoad <- function(libname, pkgname) {
    suppressWarnings(suppressMessages(tryCatch({
        conn <- acquireDuckDBConn()
        loadExtension(conn, "spatial", optional = TRUE)
        .setAlwaysXY(conn)
    }, error = function(e) invisible(NULL)
    )))
}
