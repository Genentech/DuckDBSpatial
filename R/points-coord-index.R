### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coord-indexed points: Morton (Z-order) sort-on-write for row-group pruning
###
### A spatial-transcriptomics points table is (x, y, gene) rows. A viewport
### query is a bounding-box range predicate (x BETWEEN .. AND y BETWEEN ..).
### Written in acquisition order, every Parquet row group's x/y min-max spans
### the whole slide, so no group can be skipped and a viewport query scans the
### entire file. Sorting the points by a space-filling (Morton / Z-order) curve
### before writing makes each row group cover a tight spatial tile, so DuckDB's
### zonemap pruning skips groups outside the box.
###
### This is the spatial (2-D) special case of DuckDBDataFrame's general
### `cluster_by` capability: the functions here are thin consumers of `zorder()`
### / `clusterSort()` (the shared Morton generator), so there is a single Morton
### implementation in the stack. This is a pure row reordering (no schema
### change, round-trips identically). The prunable query that exploits it is
### `layerBboxRange()` (a plain range predicate that pushes into the scan,
### unlike the `ST_Point`/`ST_Intersects` `layerSubsetByBbox`), which lives with
### the other layer-level engines in `DuckDBDataFrame-spatial.R`.

#' Reorder a points table by a Morton (Z-order) spatial key
#'
#' Returns \code{df} with its rows reordered by the Morton code of
#' \code{(x_col, y_col)} (or \code{(x_col, y_col, z_col)} when \code{z_col} is
#' given and present), so that spatially neighboring points become contiguous.
#' Writing the reordered table to Parquet gives each row group a tight
#' coordinate zonemap, which lets DuckDB prune groups outside a viewport query
#' (see \code{\link{writeSpatialPointsParquet}} and
#' \code{\link{layerBboxRange}}). A pure row permutation: no column is added, so
#' it round-trips identically. A no-op (returns \code{df} unchanged) when
#' \code{df} is empty or lacks a required coordinate column.
#'
#' This is the low-dimensional spatial convenience wrapper over
#' \code{\link[DuckDBDataFrame]{clusterSort}}\code{(df, }\code{\link[DuckDBDataFrame]{zorder}}\code{(c(x_col, y_col)))};
#' the Morton math lives in DuckDBDataFrame and is already N-D, so
#' \code{(x, y, z)} is a genuine 3-D Morton curve, not a 2-D curve with \code{z}
#' appended.
#'
#' @param df A \code{data.frame} or \link[S4Vectors:DataFrame]{DataFrame} of
#'   points.
#' @param x_col,y_col Coordinate column names (default \code{"x"}/\code{"y"}).
#' @param z_col Optional third coordinate column for 3-D clustering; used only
#'   when non-\code{NULL} and present in \code{df} (default \code{NULL}, 2-D).
#' @param bits Grid resolution per axis (default 16, a \eqn{2^{16}} grid).
#'
#' @return \code{df} reordered by Morton code (same class, same columns).
#'
#' @examples
#' df <- data.frame(x = c(9, 1, 5), y = c(9, 1, 5), gene = c("C", "A", "B"))
#' spatialSortPoints(df)
#'
#' @export
#' @importFrom DuckDBDataFrame clusterSort zorder
spatialSortPoints <- function(df, x_col = "x", y_col = "y", z_col = NULL,
                              bits = 16L) {
    if (!nrow(df) || !all(c(x_col, y_col) %in% colnames(df))) {
        return(df)
    }
    cols <- c(x_col, y_col)
    if (!is.null(z_col) && z_col %in% colnames(df)) {
        cols <- c(cols, z_col)
    }
    clusterSort(df, zorder(cols, bits = bits))
}

#' Write a coord-indexed (Morton-sorted) points Parquet
#'
#' Writes a points table to a single Parquet file, Morton-sorting it first (by
#' default) so viewport queries prune row groups. The write goes through the
#' shared DuckDBDataFrame connection and
#' \code{\link[DuckDBDataFrame]{buildParquetCopySQL}} with a bounded
#' \code{ROW_GROUP_SIZE} (so there are groups to prune). With
#' \code{spatial_sort = FALSE} the acquisition-order baseline is written
#' instead.
#'
#' @param df A \code{data.frame} / \link[S4Vectors:DataFrame]{DataFrame} of
#'   points (must contain \code{x_col} and \code{y_col}; any other columns,
#'   e.g. gene, are carried through).
#' @param path Output Parquet file path.
#' @param x_col,y_col Coordinate column names (default \code{"x"}/\code{"y"}).
#' @param z_col Optional third coordinate column for 3-D Morton clustering.
#'   Default \code{NULL} auto-detects a \code{"z"} column (so 3-D points cluster
#'   in 3-D automatically, matching scibis); pass a name to override or
#'   \code{NA} to force 2-D.
#' @param spatial_sort If \code{TRUE} (default), Morton-sort before writing
#'   (coord-indexed layout); if \code{FALSE}, write in input order (baseline).
#' @param row_group_size Parquet \code{ROW_GROUP_SIZE} (default 131072); smaller
#'   groups give finer pruning at a small metadata cost.
#' @param bits Morton grid resolution per axis (default 16).
#'
#' @return \code{path}, invisibly.
#'
#' @examples
#' df <- data.frame(x = runif(1000, 0, 100), y = runif(1000, 0, 100))
#' p <- tempfile(fileext = ".parquet")
#' writeSpatialPointsParquet(df, p)
#' unlink(p)
#'
#' @export
#' @importFrom DuckDBDataFrame acquireDuckDBConn buildParquetCopySQL
#' @importFrom DBI dbWriteTable dbRemoveTable dbExecute dbQuoteIdentifier
writeSpatialPointsParquet <-
function(df, path, x_col = "x", y_col = "y", z_col = NULL, spatial_sort = TRUE,
         row_group_size = 131072L, bits = 16L)
{
    df <- as.data.frame(df)
    if (!all(c(x_col, y_col) %in% colnames(df))) {
        stop("'df' lacks coordinate columns '", x_col, "'/'", y_col, "'")
    }
    if (spatial_sort) {
        # Auto-detect a "z" column for 3-D clustering (matches scibis's writer,
        # preserving byte-parity); z_col = NA forces 2-D, a name overrides.
        zc <- z_col
        if (is.null(zc) && "z" %in% colnames(df)) {
            zc <- "z"
        } else if (length(zc) == 1L && is.na(zc)) {
            zc <- NULL
        }
        df <- spatialSortPoints(df, x_col = x_col, y_col = y_col,
                                z_col = zc, bits = bits)
    }
    conn <- acquireDuckDBConn()
    view <- basename(tempfile("spatial_points_"))
    dbWriteTable(conn, view, df, temporary = TRUE, overwrite = TRUE)
    on.exit(try(dbRemoveTable(conn, view), silent = TRUE), add = TRUE)
    sql <- buildParquetCopySQL(
        sprintf("SELECT * FROM %s", dbQuoteIdentifier(conn, view)),
        path, row_group_size = row_group_size)
    dbExecute(conn, sql)
    invisible(path)
}
