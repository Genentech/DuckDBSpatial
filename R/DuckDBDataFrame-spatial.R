### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Layer-level spatial engines for DuckDBDataFrame (no container generics)
###

# A query returns the rows of a layer in scan order, but a layer with a named
# key is read in the order of its stored keys (rownames()), which is neither
# the scan order nor any order a database can reproduce (for character keys it
# is not even sorted).
#' @importFrom DuckDBDataFrame keycols keynames has_row_number
#' @importFrom stats setNames
.key_positions <- function(x, key_values) {
    kc <- keycols(x)[[1L]]
    kv <- as.character(key_values)
    nm <- if (is.null(names(kc))) kv else unname(setNames(names(kc), kc)[kv])
    match(nm, rownames(x))
}

# The key column of a layer whose rows come back keyed, or NULL when the
# layer is numbered by row (scan order is its order).
.layer_key <- function(x) {
    if (has_row_number(x))
        return(NULL)
    kn <- keynames(x)
    if (length(kn) != 1L)
        stop("'x' must have a single key column")
    kn
}

#' @importFrom DBI dbGetQuery
#' @importFrom dbplyr sql_render
#' @importFrom dplyr mutate pull sql
#' @importFrom DuckDBDataFrame dbconn tblconn
#' @importFrom sf st_as_sfc st_as_text st_intersects
.spatialMatch_sql <- function(x, table, coords, geom, join_fun) {
    conn <- dbconn(x)
    x_q <- sql_render(tblconn(x, select = FALSE))
    t_q <- sql_render(tblconn(table, select = FALSE))
    pred <- .st_predicate_sql_name(join_fun)
    pt_expr <- sprintf('ST_Point(n."%s", n."%s")', coords[1L], coords[2L])
    geom_ref <- .geom_col_sql_ref(conn, t_q, geom, prefix = "t")
    x_key <- .layer_key(x)
    t_key <- .layer_key(table)
    x_from <- if (is.null(x_key)) {
        sprintf("(SELECT *, row_number() OVER () AS __rowid__ FROM (%s)) n", x_q)
    } else {
        sprintf("(%s) n", x_q)
    }
    x_id <- if (is.null(x_key)) "n.__rowid__" else sprintf('n."%s"', x_key)
    t_from <- if (is.null(t_key)) {
        sprintf("(SELECT *, row_number() OVER () AS __rowid__ FROM (%s)) t", t_q)
    } else {
        sprintf("(%s) t", t_q)
    }
    t_id <- if (is.null(t_key)) "t.__rowid__" else sprintf('t."%s"', t_key)
    sql_txt <- if (is.null(t_key)) {
        sprintf(paste0("SELECT %s AS id_x, MIN(%s) AS id_t FROM %s JOIN %s ",
                       "ON %s(%s, %s) GROUP BY %s"),
                x_id, t_id, x_from, t_from, pred, pt_expr, geom_ref, x_id)
    } else {
        sprintf(paste0("SELECT DISTINCT %s AS id_x, %s AS id_t FROM %s JOIN %s ",
                       "ON %s(%s, %s)"),
                x_id, t_id, x_from, t_from, pred, pt_expr, geom_ref)
    }
    pairs <- dbGetQuery(conn, sql_txt)
    out <- rep(NA_integer_, nrow(x))
    if (nrow(pairs)) {
        xpos <- if (is.null(x_key)) as.integer(pairs$id_x)
                else .key_positions(x, pairs$id_x)
        tpos <- if (is.null(t_key)) as.integer(pairs$id_t)
                else .key_positions(table, pairs$id_t)
        # the first match is the smallest position, and the last assignment
        # to an element wins
        o <- order(tpos, decreasing = TRUE)
        out[xpos[o]] <- tpos[o]
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
#' objects. Accepts coordinate columns or a geometry column. The \code{geom}
#' column does not need to already be DuckDB-native \code{GEOMETRY}: a plain
#' WKT-text column (e.g. from a CSV, or any non-GeoParquet source) is
#' detected automatically via a cheap schema probe and cast as needed.
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
#' @importFrom dplyr collect select all_of
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
        key <- .layer_key(x)
        if (is.null(key)) {
            as.logical(pull(mutate(conn, !!!hit_expr), ".hit"))
        } else {
            res <- collect(select(mutate(conn, !!!hit_expr),
                                  all_of(c(key, ".hit"))))
            pos <- .key_positions(x, res[[key]])
            hit <- as.logical(res[[".hit"]])
            out <- rep(NA, nrow(x))
            out[pos] <- hit
            out
        }
    } else {
        geom_col <- x[[geom]]
        if (is(geom_col, "DuckDBColumn")) {
            geom_col <- .geom_column_as_geometry(geom_col)
            as.logical(as.vector(st_intersects(geom_col, y)))
        } else {
            as.logical(as.vector(sf::st_intersects(geom_col, y)))
        }
    }
}

# Same WKT-vs-GEOMETRY probe as .geom_col_sql_ref() (DuckDBTable-spatial.R),
# but for a DuckDBColumn: casts via sql_call() instead of raw SQL text, so
# the result stays a lazy DuckDBColumn ready to hand to st_intersects()
# rather than a bare SQL fragment.
#' @importFrom DuckDBDataFrame tblconn dbconn sql_call
#' @importFrom dbplyr sql_render
.geom_column_as_geometry <- function(col) {
    colname <- names(col@table@datacols)[1L]
    query_sql <- sql_render(tblconn(col, select = FALSE))
    if (.geom_col_is_native(dbconn(col), query_sql, colname))
        col
    else
        sql_call(col, "ST_GeomFromText")
}

#' Match points in a lazy table layer to a geometry table
#'
#' When \code{table} is a \code{DuckDBTable}/\code{DuckDBDataFrame}, its
#' \code{geom} column does not need to already be DuckDB-native
#' \code{GEOMETRY}: a plain WKT-text column (e.g. from a CSV, or any
#' non-GeoParquet source) is detected automatically via a cheap schema probe
#' and cast as needed.
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
#' shapes <- shapes[!is.na(shapes$type), ]
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
#' outside the box. Pruning needs a layer with a named key, as read from a file
#' with \code{DuckDBDataFrame(path, keycol = ...)} or by \code{readParquet()};
#' on a layer numbered by row the filter is applied after the numbering and
#' is not pushed into the scan. The result is a \strong{lazy} \code{DuckDBDataFrame} (nothing is
#' materialized until collected), the R counterpart of scibis' \code{query_points}.
#'
#' @param x A \link[DuckDBDataFrame:DuckDBDataFrame-class]{DuckDBDataFrame} point layer.
#' @param xmin,xmax,ymin,ymax Bounding-box limits (inclusive).
#' @param x_col,y_col Coordinate column names (default \code{"x"}/\code{"y"}).
#' @param zmin,zmax Optional inclusive depth range for a 3-D viewport; when both
#'   are non-\code{NULL} a \code{z_col BETWEEN zmin AND zmax} term is ANDed on
#'   (and pushes into the scan too, so a 3-D layout prunes on \code{z} as well).
#' @param z_col Depth column name (default \code{"z"}), used only with
#'   \code{zmin}/\code{zmax}.
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
layerBboxRange <- function(x, xmin, xmax, ymin, ymax, x_col = "x", y_col = "y",
                           zmin = NULL, zmax = NULL, z_col = "z") {
    if (!is(x, "DuckDBDataFrame")) {
        stop("'x' must be a DuckDBDataFrame")
    }
    use_z <- !is.null(zmin) && !is.null(zmax)
    need <- if (use_z) c(x_col, y_col, z_col) else c(x_col, y_col)
    if (!all(need %in% colnames(x))) {
        stop("'x' lacks coordinate columns '", paste(need, collapse = "'/'"), "'")
    }
    xv <- x[[x_col]]
    yv <- x[[y_col]]
    keep <- xv >= xmin & xv <= xmax & yv >= ymin & yv <= ymax
    if (use_z) {
        zv <- x[[z_col]]
        keep <- keep & zv >= zmin & zv <= zmax
    }
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
