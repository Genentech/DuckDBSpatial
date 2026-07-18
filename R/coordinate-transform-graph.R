### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coordinate-transform graph: resolve a transform between coordinate systems
###
### A dataset's coordinate systems and the transforms between them form a
### directed graph (RFC-5 `coordinateTransformations` edges): nodes are
### coordinate-system names, an edge carries a CoordinateTransform mapping its
### input system to its output system. To move points from one coordinate system
### to another you find a path and compose the edge transforms; an invertible
### edge is also traversable backwards (via `ctInvert()`), so `ctGraph()` adds
### the reverse edge. This is the R twin of SpatialData's transformation graph,
### built on the transform algebra in coordinate-transforms.R with base R only
### (a named-list adjacency + a small path search -- no graph/RBGL/Rgraphviz
### dependency).

# Normalize a `systems` argument (named list of coordinateSystem objects,
# axis-name vectors, or RFC-5 axis lists) into a named list of axis-name
# character vectors.
.ctAxisNames <- function(systems) {
    if (is.null(systems)) return(NULL)
    lapply(systems, function(s) {
        if (inherits(s, "CoordinateSystem")) return(s$axes)
        if (is.character(s)) return(s)
        # a list of axis specs {name,type,unit}
        vapply(s, function(a) if (is.list(a)) a$name else as.character(a),
               character(1L))
    })
}

#' Define a named coordinate system
#'
#' A coordinate system is a name plus an ordered set of axes (RFC-5
#' \code{{name, type, unit}}). Coordinate transforms map points between
#' coordinate systems; see \code{\link{ctGraph}}.
#'
#' @param name Coordinate-system name.
#' @param axes A character vector of axis names, or a list of axis specs each
#'   with \code{name} (and optional \code{type}, \code{unit}).
#' @return A \code{CoordinateSystem}.
#' @examples
#' coordinateSystem("global", c("y", "x"))
#' @export
coordinateSystem <- function(name, axes) {
    if (is.character(axes)) {
        spec <- lapply(axes, function(a) list(name = a, type = "space"))
        axis_names <- axes
    } else {
        spec <- axes
        axis_names <- vapply(axes,
            function(a) if (is.list(a)) a$name else as.character(a),
            character(1L))
    }
    structure(list(name = name, axes = axis_names, spec = spec),
              class = "CoordinateSystem")
}

#' Build a coordinate-transform graph
#'
#' Assembles the directed graph of coordinate systems and the transforms between
#' them. Each invertible edge gets an automatically added reverse edge (via
#' \code{\link{ctInvert}}), so a target reachable by inversion is found by
#' \code{\link{ctPath}}. Mirrors SpatialData's transformation graph.
#'
#' @param edges A list of edges, each \code{list(input=, output=, transform=)}
#'   where \code{input}/\code{output} are coordinate-system names and
#'   \code{transform} is a \code{\link{coordinate-transforms}} object or dict.
#' @param systems Optional named list of coordinate systems
#'   (\code{\link{coordinateSystem}} objects or axis-name vectors), keyed by
#'   name. Supplies axis orders for \code{\link{ctBetween}} and the set of valid
#'   nodes (so a path to/from an undefined system errors).
#' @return A \code{CTgraph}.
#' @examples
#' g <- ctGraph(list(list(input = "a", output = "b",
#'              transform = ctScale(c(2, 2)))),
#'              systems = list(a = c("y", "x"), b = c("y", "x")))
#' ctPath(g, "a", "b")
#' @export
ctGraph <- function(edges, systems = NULL) {
    axes <- .ctAxisNames(systems)
    adj <- list()
    add <- function(from, to, transform) {
        adj[[from]] <<- c(adj[[from]],
                          list(list(to = to, transform = transform)))
    }
    for (e in edges) {
        tf <- asCoordinateTransform(e$transform)
        add(e$input, e$output, tf)
        inv <- tryCatch(ctInvert(tf), error = function(err) NULL)
        if (!is.null(inv)) add(e$output, e$input, inv)
    }
    nodes <- unique(c(names(axes),
                      unlist(lapply(edges, function(e) c(e$input, e$output)))))
    structure(list(nodes = nodes, systems = axes, adj = adj),
              class = "CTgraph")
}

# Enumerate all simple paths from `from` to `to` as lists of edges.
.ctSimplePaths <- function(adj, from, to) {
    results <- list()
    walk <- function(node, visited, acc) {
        if (identical(node, to)) {
            results[[length(results) + 1L]] <<- acc
            return()
        }
        for (e in adj[[node]]) {
            if (!(e$to %in% visited))
                walk(e$to, c(visited, e$to), c(acc, list(e)))
        }
    }
    walk(from, from, list())
    results
}

#' Resolve the transform between two coordinate systems
#'
#' Finds the shortest path in a \code{\link{ctGraph}} from \code{from} to
#' \code{to} and returns the composed \code{\link{coordinate-transforms}}
#' transform (a \code{ctSequence} of the edge transforms, or the single edge
#' transform for a one-hop path, or \code{ctIdentity()} when \code{from == to}).
#' Errors if either coordinate system is unknown, if no path exists, or if two
#' distinct shortest paths tie (an ambiguous resolution).
#'
#' @param graph A \code{CTgraph}.
#' @param from,to Coordinate-system names.
#' @return A \code{CoordinateTransform}.
#' @examples
#' g <- ctGraph(list(list(input = "a", output = "b",
#'              transform = ctScale(c(2, 2)))),
#'              systems = list(a = c("y", "x"), b = c("y", "x")))
#' ctPath(g, "b", "a")  # traverses the auto-added inverse edge
#' @export
ctPath <- function(graph, from, to) {
    if (!(from %in% graph$nodes))
        stop("unknown coordinate system: ", from)
    if (!(to %in% graph$nodes))
        stop("unknown coordinate system: ", to)
    if (identical(from, to)) return(ctIdentity())
    paths <- .ctSimplePaths(graph$adj, from, to)
    if (!length(paths))
        stop("no coordinate-transform path from '", from, "' to '", to, "'")
    lens <- lengths(paths)
    shortest <- paths[lens == min(lens)]
    if (length(shortest) > 1L)
        stop("ambiguous coordinate-transform path from '", from, "' to '", to,
             "': ", length(shortest), " shortest paths tie")
    edges <- shortest[[1L]]
    transforms <- lapply(edges, function(e) e$transform)
    if (length(transforms) == 1L) transforms[[1L]] else ctSequence(transforms)
}

#' Transform points between two coordinate systems
#'
#' Resolves the transform with \code{\link{ctPath}} and applies it to a matrix
#' of points with \code{\link{ctApply}}, using the axis orders recorded for the
#' two coordinate systems in the graph.
#'
#' @param graph A \code{CTgraph} built with \code{systems} (so axis orders are
#'   known).
#' @param from,to Coordinate-system names.
#' @param points A numeric matrix (rows = points) or a vector (one point).
#' @return A numeric matrix of transformed points.
#' @examples
#' g <- ctGraph(list(list(input = "a", output = "b",
#'              transform = ctScale(c(2, 3)))),
#'              systems = list(a = c("y", "x"), b = c("y", "x")))
#' ctBetween(g, "a", "b", rbind(c(1, 1)))
#' @export
ctBetween <- function(graph, from, to, points) {
    if (is.null(graph$systems) || is.null(graph$systems[[from]]) ||
        is.null(graph$systems[[to]]))
        stop("ctBetween needs axis orders; build ctGraph() with 'systems'")
    tf <- ctPath(graph, from, to)
    ctApply(tf, points, graph$systems[[from]], graph$systems[[to]])
}

#' @export
print.CTgraph <- function(x, ...) {
    cat("<CTgraph: ", length(x$nodes), " coordinate systems, ",
        sum(lengths(x$adj)), " edges (incl. inverses)>\n", sep = "")
    invisible(x)
}

#' @export
print.CoordinateSystem <- function(x, ...) {
    cat("<CoordinateSystem '", x$name, "': ", paste(x$axes, collapse = ", "),
        ">\n", sep = "")
    invisible(x)
}
