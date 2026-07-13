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
# `MODL_DUCKDB_EXTENSION_REPOSITORY` at a reachable mirror.

#' @importFrom DuckDBDataFrame acquireDuckDBConn loadExtension
.onLoad <- function(libname, pkgname) {
    suppressWarnings(suppressMessages(tryCatch(
        loadExtension(acquireDuckDBConn(), "spatial", optional = TRUE),
        error = function(e) invisible(NULL)
    )))
}
