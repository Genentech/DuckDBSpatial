### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Layer-level spatial engines for DuckDBDataFrame (no container generics)
###

#' @importFrom DBI dbGetQuery
#' @importFrom dbplyr sql_render
#' @importFrom dplyr mutate pull sql
#' @importFrom DuckDBDataFrame DuckDBDataFrame dbconn tblconn
#' @importFrom sf st_as_sfc st_as_text st_intersects
.spatialMatch_sql <- function(x, table, coords, geom, join_fun) {
    conn <- dbconn(x)
    x_q <- sql_render(tblconn(x, select = FALSE))
    if (is(table, "DuckDBTable")) {
        t_q <- sql_render(tblconn(table, select = FALSE))
    } else {
        tmp <- DuckDBDataFrame(as.data.frame(table))
        t_q <- sql_render(tblconn(tmp, select = FALSE))
    }
    pred <- if (identical(join_fun, sf::st_within)) "ST_Within" else "ST_Intersects"
    pt_expr <- sprintf('ST_Point(n."%s", n."%s")', coords[1L], coords[2L])
    sql_txt <- sprintf(
        paste0("WITH numbered AS (SELECT *, row_number() OVER () AS __rowid__ FROM (%s)) ",
               "SELECT n.__rowid__ AS id_x, MIN(t.__rowid__) AS match_idx ",
               "FROM numbered n JOIN (SELECT *, row_number() OVER () AS __rowid__ FROM (%s)) t ",
               "ON %s(%s, t.\"%s\") GROUP BY n.__rowid__"),
        x_q, t_q, pred, pt_expr, geom)
    pairs <- dbGetQuery(conn, sql_txt)
    n <- nrow(x)
    out <- rep(NA_integer_, n)
    if (nrow(pairs)) {
        out[as.integer(pairs$id_x)] <- as.integer(pairs$match_idx)
    }
    out
}

.overlaps_inmemory <- function(x, y, coords = NULL, geom = "geometry") {
    if (!is.null(coords)) {
        pts <- st_as_sfc(paste0("POINT(", x[[coords[1L]]], " ", x[[coords[2L]]], ")"))
        as.logical(lengths(st_intersects(pts, y)) > 0L)
    } else {
        as.logical(lengths(st_intersects(x[[geom]], y)) > 0L)
    }
}

#' Test spatial intersection for a lazy table layer
#'
#' Layer-level intersection predicate for \link[DuckDBDataFrame:DuckDBDataFrame-class]{DuckDBDataFrame}
#' objects. Accepts coordinate columns or a geometry column.
#'
#' @param x A \code{DuckDBDataFrame} layer.
#' @param y A geometry (\code{sfc}, \code{sfg}), WKT character, or \code{dplyr::sql} expression.
#' @param coords Optional length-two character vector of x/y column names.
#' @param geom Geometry column name when \code{coords} is \code{NULL}.
#'
#' @return Logical vector of length \code{nrow(x)}.
#'
#' @examples
#' pts_path <- tempfile(fileext = ".csv")
#' on.exit(unlink(pts_path), add = TRUE)
#' write.csv(data.frame(x = c(1, 5, 10), y = c(1, 5, 10)), pts_path, row.names = FALSE)
#' pts <- DuckDBDataFrame(pts_path, datacols = c("x", "y"))
#' poly <- sf::st_as_sfc("POLYGON((0 0, 6 0, 6 6, 0 6, 0 0))")
#' layerSpatialOverlaps(pts, poly, coords = c("x", "y"))
#'
#' @export
#' @importClassesFrom DuckDBDataFrame DuckDBDataFrame DuckDBColumn
#' @importFrom DuckDBDataFrame tblconn
#' @importFrom stats setNames
layerSpatialOverlaps <- function(x, y, coords = NULL, geom = "geometry") {
    if (!is(x, "DuckDBDataFrame"))
        stop("'x' must be a DuckDBDataFrame")
    y_sql <- .geom_to_sql(y)
    if (!is.null(coords)) {
        conn <- tblconn(x, select = FALSE)
        bool_sql <- sprintf("ST_Intersects(ST_Point(\"%s\", \"%s\"), %s)",
                            coords[1L], coords[2L], as.character(y_sql))
        hit_expr <- setNames(list(sql(bool_sql)), ".hit")
        as.logical(pull(mutate(conn, !!!hit_expr), ".hit"))
    } else {
        geom_col <- x[[geom]]
        if (is(geom_col, "DuckDBColumn")) {
            as.logical(as.vector(st_intersects(geom_col, y)))
        } else {
            as.logical(as.vector(sf::st_intersects(geom_col, y)))
        }
    }
}

#' Match points in a lazy table layer to a geometry table
#'
#' @param x A \code{DuckDBDataFrame} point layer.
#' @param table A \code{DuckDBDataFrame} or in-memory \code{DataFrame} with geometries.
#' @param coords Length-two character vector of x/y column names in \code{x}.
#' @param geom Geometry column name in \code{table}.
#' @param join Optional \code{sf} predicate (default \code{sf::st_intersects}).
#'
#' @return Integer vector of match indices (NA where unmatched).
#'
#' @examples
#' spatial_path <- system.file("extdata", "spatial", package = "DuckDBSpatial")
#' shapes <- DuckDBDataFrame(spatial_path)
#' shapes <- shapes[which(!is.na(shapes$type)), ]
#' pts_path <- tempfile(fileext = ".csv")
#' on.exit(unlink(pts_path), add = TRUE)
#' write.csv(data.frame(x = c(1, 5, 30), y = c(1, 5, 10)), pts_path, row.names = FALSE)
#' pts <- DuckDBDataFrame(pts_path, datacols = c("x", "y"))
#' layerSpatialMatch(pts, shapes, coords = c("x", "y"))
#'
#' @export
#' @importClassesFrom DuckDBDataFrame DuckDBDataFrame
#' @importClassesFrom S4Vectors DataFrame
layerSpatialMatch <- function(x, table, coords, geom = "geometry", join = NULL) {
    if (!is(x, "DuckDBDataFrame"))
        stop("'x' must be a DuckDBDataFrame")
    if (is.null(join))
        join <- sf::st_intersects
    if (is(table, "DuckDBTable"))
        return(.spatialMatch_sql(x, table, coords, geom, join))
    x_coords <- as.data.frame(x[, coords, drop = FALSE])
    pts_sfc <- st_as_sfc(paste0("POINT(", x_coords[[coords[1L]]], " ",
                               x_coords[[coords[2L]]], ")"))
    tbl_geom <- table[[geom]]
    res <- join(pts_sfc, tbl_geom)
    n <- nrow(x)
    out <- integer(n)
    for (i in seq_len(n)) {
        idx <- res[[i]]
        out[i] <- if (length(idx) > 0L) idx[1L] else NA_integer_
    }
    out
}

#' Row indices of a layer inside a bounding box
#'
#' @param x A \code{DuckDBDataFrame} or in-memory tabular layer.
#' @param xmin,xmax,ymin,ymax Bounding box limits.
#' @param x_col,y_col Coordinate column names for point layers.
#' @param geom Geometry column name for polygon layers.
#'
#' @return Integer row indices.
#'
#' @examples
#' pts_path <- tempfile(fileext = ".csv")
#' on.exit(unlink(pts_path), add = TRUE)
#' write.csv(data.frame(x = c(1, 5, 10), y = c(1, 5, 10)), pts_path, row.names = FALSE)
#' pts <- DuckDBDataFrame(pts_path, datacols = c("x", "y"))
#' layerSubsetByBbox(pts, 0, 10, 0, 10, x_col = "x", y_col = "y")
#'
#' @export
layerSubsetByBbox <- function(x, xmin, xmax, ymin, ymax,
                              x_col = "x", y_col = "y", geom = "geometry") {
    if (is(x, "DuckDBDataFrame")) {
        if (!is.null(x_col) && x_col %in% colnames(x)) {
            env <- st_make_envelope_sql(xmin, ymin, xmax, ymax)
            which(layerSpatialOverlaps(x, env, coords = c(x_col, y_col)))
        } else if (geom %in% colnames(x)) {
            env <- st_make_envelope_sql(xmin, ymin, xmax, ymax)
            which(layerSpatialOverlaps(x, env, geom = geom))
        } else {
            integer(0L)
        }
    } else {
        if (!is.null(x_col) && x_col %in% colnames(x)) {
            xv <- x[[x_col]]
            yv <- x[[y_col]]
            which((xv >= xmin & xv <= xmax) & (yv >= ymin & yv <= ymax))
        } else {
            integer(0L)
        }
    }
}

#' Row indices of a layer intersecting a geometry
#'
#' @param x A \code{DuckDBDataFrame} or in-memory tabular layer.
#' @param y A geometry to test against.
#' @param coords Optional x/y column names for point layers.
#' @param geom Geometry column name for polygon layers.
#'
#' @return Integer row indices.
#'
#' @examples
#' pts_path <- tempfile(fileext = ".csv")
#' on.exit(unlink(pts_path), add = TRUE)
#' write.csv(data.frame(x = c(1, 5, 10), y = c(1, 5, 10)), pts_path, row.names = FALSE)
#' pts <- DuckDBDataFrame(pts_path, datacols = c("x", "y"))
#' poly <- sf::st_as_sfc("POLYGON((0 0, 6 0, 6 6, 0 6, 0 0))")
#' layerSubsetByGeometry(pts, poly, coords = c("x", "y"))
#'
#' @export
layerSubsetByGeometry <- function(x, y, coords = NULL, geom = "geometry") {
    if (is(x, "DuckDBDataFrame")) {
        which(layerSpatialOverlaps(x, y, coords = coords, geom = geom))
    } else {
        which(.overlaps_inmemory(x, y, coords = coords, geom = geom))
    }
}

#' Subset a points layer to a bounding box with a prunable range predicate
#'
#' Returns the rows of point layer \code{x} inside \code{[xmin, xmax] x [ymin, ymax]}
#' using a plain coordinate range predicate
#' (\code{x_col >= xmin & x_col <= xmax & y_col >= ymin & y_col <= ymax}). Unlike
#' \code{\link{layerSubsetByBbox}} (which builds a per-row \code{ST_Point} and tests
#' \code{ST_Intersects} against an envelope, and returns row indices), the range
#' predicate pushes into DuckDB's Parquet reader, so on a Morton-sorted (coord-indexed)
#' points file (see \code{\link{writeSpatialPointsParquet}}) it prunes row groups
#' outside the box. The result is a \strong{lazy} \code{DuckDBDataFrame} (nothing is
#' materialized until collected), the R counterpart of scibis' \code{query_points}.
#'
#' @param x A \link[DuckDBDataFrame:DuckDBDataFrame-class]{DuckDBDataFrame} point layer.
#' @param xmin,xmax,ymin,ymax Bounding-box limits (inclusive).
#' @param x_col,y_col Coordinate column names (default \code{"x"}/\code{"y"}).
#'
#' @return A lazy \code{DuckDBDataFrame} filtered to the box.
#'
#' @examples
#' p <- tempfile(fileext = ".parquet")
#' writeSpatialPointsParquet(
#'     data.frame(x = c(1, 5, 50), y = c(1, 5, 50), gene = c("A", "B", "C")), p)
#' pts <- DuckDBDataFrame::DuckDBDataFrame(p)
#' as.data.frame(layerBboxRange(pts, 0, 10, 0, 10))
#' unlink(p)
#'
#' @export
#' @importClassesFrom DuckDBDataFrame DuckDBDataFrame
layerBboxRange <- function(x, xmin, xmax, ymin, ymax, x_col = "x", y_col = "y") {
    if (!is(x, "DuckDBDataFrame")) {
        stop("'x' must be a DuckDBDataFrame")
    }
    if (!all(c(x_col, y_col) %in% colnames(x))) {
        stop("'x' lacks coordinate columns '", x_col, "'/'", y_col, "'")
    }
    xv <- x[[x_col]]
    yv <- x[[y_col]]
    keep <- xv >= xmin & xv <= xmax & yv >= ymin & yv <= ymax
    x[keep, ]
}

#' Apply a coordinate transform to a spatial layer
#'
#' Transforms a spatial layer by a 2-D affine coordinate transform, in place and
#' lazily (nothing is materialized until collected). \code{transform} is a
#' \code{\link{coordinate-transforms}} object (\code{ctScale},
#' \code{ctTranslation}, \code{ctRotation}, \code{ctAffine}, \code{ctSequence},
#' …) or an affine matrix, lowered to a 2-D affine (\eqn{x' = a x + b y + xoff},
#' \eqn{y' = d x + e y + yoff}). A point layer (columns \code{x_col}/
#' \code{y_col}) is transformed by arithmetic on those columns; a geometry layer
#' (column \code{geom}) by \code{\link{st_affine}} (DuckDB \code{ST_Affine}). To
#' move a layer between named coordinate systems, resolve the transform with
#' \code{\link{ctPath}} first and pass it here.
#'
#' @param x A \link[DuckDBDataFrame:DuckDBDataFrame-class]{DuckDBDataFrame}
#'   layer.
#' @param transform A \code{CoordinateTransform} or affine matrix (must be
#'   2-D-expressible; a dimensionality change errors).
#' @param x_col,y_col Coordinate column names for point layers (default
#'   \code{"x"}/\code{"y"}); set \code{x_col = NULL} to force the geometry path.
#' @param geom Geometry column name for geometry layers (default
#'   \code{"geometry"}).
#'
#' @return A lazy \code{DuckDBDataFrame} with transformed coordinates.
#'
#' @seealso \code{\link{coordinate-transforms}}, \code{\link{ctGraph}},
#'   \code{\link{st_affine}}
#'
#' @examples
#' p <- tempfile(fileext = ".parquet")
#' writeSpatialPointsParquet(data.frame(x = c(1, 2, 3), y = c(0, 0, 0)), p)
#' pts <- DuckDBDataFrame::DuckDBDataFrame(p)
#' # rotate 90 degrees about the origin: (x, y) -> (-y, x)
#' as.data.frame(transformLayer(pts, ctRotation(rbind(c(0, -1), c(1, 0)))))
#' unlink(p)
#'
#' @export
#' @importClassesFrom DuckDBDataFrame DuckDBDataFrame
transformLayer <- function(x, transform, x_col = "x", y_col = "y",
                           geom = "geometry") {
    if (!is(x, "DuckDBDataFrame")) {
        stop("'x' must be a DuckDBDataFrame")
    }
    co <- .ctTo2DAffine(transform)
    cols <- colnames(x)
    if (!is.null(x_col) && !is.null(y_col) &&
        x_col %in% cols && y_col %in% cols) {
        xv <- x[[x_col]]
        yv <- x[[y_col]]
        x[[x_col]] <- co$a * xv + co$b * yv + co$xoff
        x[[y_col]] <- co$d * xv + co$e * yv + co$yoff
        x
    } else if (!is.null(geom) && geom %in% cols) {
        x[[geom]] <- st_affine(x[[geom]], transform)
        x
    } else {
        stop("'x' has no coordinate columns ('", x_col, "'/'", y_col,
             "') or geometry column ('", geom, "') to transform")
    }
}
