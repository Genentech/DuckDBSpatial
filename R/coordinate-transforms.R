### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coordinate transforms: the OME-NGFF RFC-5 affine family as an R algebra
###
### A spatial element lives in an intrinsic coordinate system and is mapped into
### named (extrinsic) coordinate systems by coordinate transforms. RFC-5 defines
### a family of transforms -- identity, scale, translation, rotation, affine,
### sequence, mapAxis, byDimension, bijection -- every one of which is
### expressible as a (possibly non-square, dimensionality-changing) affine map.
### This file is the transform algebra: a single `ctAffineMatrix()` that lowers
### any transform to a homogeneous affine matrix, `ctApply()` that applies it to
### a coordinate matrix (the conformance oracle), and `ctInvert()` for reverse
### traversal. The coordinate-transform graph (`ctGraph()` / `ctPath()`) is
### built on top of these in coordinate-transform-graph.R; DuckDB data
### application (`st_affine()` / `transformLayer()`) lowers the 2-D case to
### `ST_Affine` SQL.
###
### Transforms are positional: a transform operates on the coordinate vector in
### the coordinate system's own axis order (RFC-5 addresses mapAxis/byDimension
### axes by integer index, and affine/scale/translation by axis position), so no
### name-based reprojection is needed. Displacement / coordinate-field
### ("by path") transforms are out of scope (they carry external arrays, not a
### matrix).

# Build a homogeneous (n_out+1) x (n_in+1) matrix from an n_out x n_in linear
# block and a length-n_out translation vector.
.ctHomog <- function(linear, translation) {
    linear <- as.matrix(linear)
    n_out <- nrow(linear)
    n_in <- ncol(linear)
    m <- matrix(0, n_out + 1L, n_in + 1L)
    m[seq_len(n_out), seq_len(n_in)] <- linear
    m[seq_len(n_out), n_in + 1L] <- translation
    m[n_out + 1L, n_in + 1L] <- 1
    m
}

# Coerce a list-of-rows or matrix into a numeric matrix.
.ctAsMatrix <- function(x) {
    if (is.matrix(x)) return(matrix(as.numeric(x), nrow(x), ncol(x)))
    do.call(rbind, lapply(x, as.numeric))
}

#' Coordinate-transform constructors (OME-NGFF RFC-5)
#'
#' Build the RFC-5 affine-family coordinate transforms. Each returns a
#' \code{CoordinateTransform} (a classed list mirroring the RFC-5 / SpatialData
#' on-disk dict), consumed by \code{\link{ctAffineMatrix}} /
#' \code{\link{ctApply}} and the coordinate-transform graph
#' (\code{\link{ctGraph}}). Transforms are positional: they act on the
#' coordinate vector in the coordinate system's axis order.
#'
#' @param scale,translation Numeric per-axis vectors.
#' @param rotation,affine A rotation (square) or affine (\eqn{n_{out} \times
#'   (n_{in}+1)}, RFC-5 non-homogeneous) matrix, as a matrix or list of rows.
#' @param map Integer 0-based axis permutation: output axis \code{i} is taken
#'   from input axis \code{map[i]}.
#' @param transformations A list of \code{CoordinateTransform}s (applied first
#'   to last) for \code{ctSequence}.
#' @param parts For \code{ctByDimension}, a list of
#'   \code{list(transformation=, input_axes=, output_axes=)} (axes 0-based),
#'   each sub-transform acting on its own axis subset.
#' @param forward,inverse \code{CoordinateTransform}s giving an explicit
#'   forward/inverse pair for \code{ctBijection}.
#'
#' @return A \code{CoordinateTransform}.
#'
#' @examples
#' ctScale(c(10, 20))
#' ctSequence(list(ctScale(c(10, 20)), ctTranslation(c(1, 1))))
#'
#' @name coordinate-transforms
NULL

#' @rdname coordinate-transforms
#' @export
ctIdentity <- function() {
    structure(list(type = "identity"), class = "CoordinateTransform")
}

#' @rdname coordinate-transforms
#' @export
ctScale <- function(scale) {
    structure(list(type = "scale", scale = as.numeric(scale)),
              class = "CoordinateTransform")
}

#' @rdname coordinate-transforms
#' @export
ctTranslation <- function(translation) {
    structure(list(type = "translation", translation = as.numeric(translation)),
              class = "CoordinateTransform")
}

#' @rdname coordinate-transforms
#' @export
ctRotation <- function(rotation) {
    structure(list(type = "rotation", rotation = .ctAsMatrix(rotation)),
              class = "CoordinateTransform")
}

#' @rdname coordinate-transforms
#' @export
ctAffine <- function(affine) {
    structure(list(type = "affine", affine = .ctAsMatrix(affine)),
              class = "CoordinateTransform")
}

#' @rdname coordinate-transforms
#' @export
ctSequence <- function(transformations) {
    transformations <- lapply(transformations, asCoordinateTransform)
    structure(list(type = "sequence", transformations = transformations),
              class = "CoordinateTransform")
}

#' @rdname coordinate-transforms
#' @export
ctMapAxis <- function(map) {
    structure(list(type = "mapAxis", mapAxis = as.integer(map)),
              class = "CoordinateTransform")
}

#' @rdname coordinate-transforms
#' @export
ctByDimension <- function(parts) {
    parts <- lapply(parts, function(p) list(
        transformation = asCoordinateTransform(p$transformation),
        input_axes = as.integer(p$input_axes),
        output_axes = as.integer(p$output_axes)))
    structure(list(type = "byDimension", transformations = parts),
              class = "CoordinateTransform")
}

#' @rdname coordinate-transforms
#' @export
ctBijection <- function(forward, inverse) {
    structure(list(type = "bijection",
                   forward = asCoordinateTransform(forward),
                   inverse = asCoordinateTransform(inverse)),
              class = "CoordinateTransform")
}

#' Coerce a transform dict into a CoordinateTransform
#'
#' Turns an RFC-5 transform dict (a named list with a \code{type} field, as read
#' from JSON) into a \code{\link{coordinate-transforms}} object, recursing
#' through \code{sequence} / \code{byDimension} / \code{bijection}. A
#' \code{CoordinateTransform} passes through unchanged.
#'
#' @param x A \code{CoordinateTransform} or a transform dict (named list).
#' @return A \code{CoordinateTransform}.
#' @export
asCoordinateTransform <- function(x) {
    if (inherits(x, "CoordinateTransform")) return(x)
    if (!is.list(x) || is.null(x$type))
        stop("not a transform: expected a list with a 'type' field")
    switch(x$type,
        identity = ctIdentity(),
        scale = ctScale(x$scale),
        translation = ctTranslation(x$translation),
        rotation = ctRotation(x$rotation),
        affine = ctAffine(x$affine),
        sequence = ctSequence(x$transformations),
        mapAxis = ctMapAxis(x$mapAxis),
        byDimension = ctByDimension(x$transformations),
        bijection = ctBijection(x$forward, x$inverse),
        stop("unsupported transform type: ", x$type))
}

# Output dimensionality of a transform given input dimensionality n_in.
.ctNout <- function(transform, n_in) {
    switch(transform$type,
        affine = nrow(transform$affine),
        rotation = nrow(transform$rotation),
        mapAxis = length(transform$mapAxis),
        scale = length(transform$scale),
        translation = length(transform$translation),
        bijection = .ctNout(transform$forward, n_in),
        n_in)  # identity, sequence (same-dim), byDimension default to n_in
}

#' Lower a coordinate transform to a homogeneous affine matrix
#'
#' Returns the homogeneous \eqn{(n_{out}+1) \times (n_{in}+1)} matrix of a
#' \code{\link{coordinate-transforms}} transform, so that
#' \eqn{[out, 1]^T = M \, [in, 1]^T}. Every RFC-5 type reduces to such a matrix;
#' \code{sequence} composes by matrix multiplication and \code{byDimension}
#' scatters its per-axis sub-transforms into the block. \code{bijection} uses
#' its forward transform.
#'
#' @param transform A \code{CoordinateTransform} (or a transform dict).
#' @param input_axes,output_axes Character vectors of the source / target axis
#'   names (only their lengths, the dimensionalities, are used).
#' @return A numeric homogeneous matrix.
#' @examples
#' ctAffineMatrix(ctScale(c(10, 20)), c("y", "x"), c("y", "x"))
#' @export
ctAffineMatrix <- function(transform, input_axes, output_axes) {
    transform <- asCoordinateTransform(transform)
    n_in <- length(input_axes)
    n_out <- length(output_axes)
    switch(transform$type,
        identity = .ctHomog(diag(n_in), rep(0, n_in)),
        scale = {
            s <- transform$scale
            .ctHomog(diag(s, length(s)), rep(0, length(s)))
        },
        translation = {
            t <- transform$translation
            .ctHomog(diag(length(t)), t)
        },
        rotation = .ctHomog(transform$rotation,
                            rep(0, nrow(transform$rotation))),
        affine = {
            a <- transform$affine
            rbind(a, c(rep(0, ncol(a) - 1L), 1))
        },
        mapAxis = {
            m <- transform$mapAxis
            linear <- matrix(0, length(m), n_in)
            for (i in seq_along(m)) linear[i, m[i] + 1L] <- 1
            .ctHomog(linear, rep(0, length(m)))
        },
        sequence = {
            m <- .ctHomog(diag(n_in), rep(0, n_in))
            cur <- n_in
            for (sub in transform$transformations) {
                nout <- .ctNout(sub, cur)
                sm <- ctAffineMatrix(sub, seq_len(cur), seq_len(nout))
                m <- sm %*% m
                cur <- nout
            }
            m
        },
        byDimension = {
            m <- matrix(0, n_out + 1L, n_in + 1L)
            m[n_out + 1L, n_in + 1L] <- 1
            for (p in transform$transformations) {
                si <- p$input_axes + 1L
                so <- p$output_axes + 1L
                sm <- ctAffineMatrix(p$transformation, si, so)
                ko <- length(so)
                ki <- length(si)
                m[so, si] <- sm[seq_len(ko), seq_len(ki), drop = FALSE]
                m[so, n_in + 1L] <- sm[seq_len(ko), ki + 1L]
            }
            m
        },
        bijection = ctAffineMatrix(transform$forward, input_axes, output_axes),
        stop("unsupported transform type: ", transform$type))
}

#' Apply a coordinate transform to a matrix of points
#'
#' Transforms an \eqn{n \times d_{in}} matrix of coordinates to \eqn{n \times
#' d_{out}} via the transform's homogeneous affine matrix
#' (\code{\link{ctAffineMatrix}}). This is the RFC-5 conformance oracle.
#'
#' @param transform A \code{CoordinateTransform} (or a transform dict).
#' @param points A numeric matrix (rows = points) or a vector (one point).
#' @param input_axes,output_axes Character vectors of source / target axis
#'   names.
#' @return A numeric matrix of transformed points (\eqn{n \times d_{out}}).
#' @examples
#' ctApply(ctTranslation(c(10, 20)), rbind(c(1, 2)), c("y", "x"), c("y", "x"))
#' @export
ctApply <- function(transform, points, input_axes, output_axes) {
    if (is.null(dim(points))) points <- matrix(points, nrow = 1L)
    points <- matrix(as.numeric(points), nrow(points), ncol(points))
    m <- ctAffineMatrix(transform, input_axes, output_axes)
    n_out <- length(output_axes)
    homog <- cbind(points, 1)
    out <- homog %*% t(m)
    out[, seq_len(n_out), drop = FALSE]
}

#' Invert a coordinate transform
#'
#' Returns the inverse \code{\link{coordinate-transforms}} transform: identity
#' is its own inverse; scale inverts elementwise; translation negates; rotation
#' / affine invert by \code{\link{solve}} (errors for a non-invertible or
#' dimensionality-changing affine); mapAxis inverts the permutation; sequence
#' reverses a list of inverses; bijection swaps its declared forward / inverse.
#' Used by \code{\link{ctGraph}} to add reverse edges.
#'
#' @param transform A \code{CoordinateTransform} (or a transform dict).
#' @return The inverse \code{CoordinateTransform}.
#' @examples
#' ctInvert(ctScale(c(10, 20)))
#' @export
ctInvert <- function(transform) {
    transform <- asCoordinateTransform(transform)
    switch(transform$type,
        identity = ctIdentity(),
        scale = ctScale(1 / transform$scale),
        translation = ctTranslation(-transform$translation),
        rotation = ctRotation(solve(transform$rotation)),
        affine = {
            a <- transform$affine
            if (nrow(a) != ncol(a) - 1L)
                stop("cannot invert a dimensionality-changing affine")
            full <- rbind(a, c(rep(0, ncol(a) - 1L), 1))
            inv <- solve(full)
            ctAffine(inv[seq_len(nrow(inv) - 1L), , drop = FALSE])
        },
        mapAxis = {
            m <- transform$mapAxis
            inv <- integer(length(m))
            inv[m + 1L] <- seq_along(m) - 1L
            ctMapAxis(inv)
        },
        sequence = ctSequence(rev(lapply(transform$transformations, ctInvert))),
        bijection = ctBijection(transform$inverse, transform$forward),
        byDimension = {
            parts <- lapply(transform$transformations, function(p) list(
                transformation = ctInvert(p$transformation),
                input_axes = p$output_axes, output_axes = p$input_axes))
            ctByDimension(parts)
        },
        stop("cannot invert transform type: ", transform$type))
}

# Extract 2-D ST_Affine coefficients (a, b, d, e, xoff, yoff) from a
# CoordinateTransform or an affine matrix (2x2 linear, 2x3 linear+offset, or 3x3
# homogeneous). x' = a*x + b*y + xoff; y' = d*x + e*y + yoff. Errors when the
# transform is not 2-D-expressible (e.g. a dimensionality change) -- the DuckDB
# data engine is 2-D. Used by st_affine() and transformLayer().
.ctTo2DAffine <- function(x) {
    if (inherits(x, "CoordinateTransform")) {
        m <- ctAffineMatrix(x, c("x", "y"), c("x", "y"))
        if (!identical(dim(m), c(3L, 3L)))
            stop("transform is not 2-D-expressible (produces ", nrow(m) - 1L,
                 "-D output); the DuckDB data engine is 2-D")
    } else {
        m <- matrix(as.numeric(as.matrix(x)), nrow(as.matrix(x)))
        if (nrow(m) == 2L && ncol(m) == 2L) m <- cbind(m, c(0, 0))
        if (nrow(m) == 2L && ncol(m) == 3L) m <- rbind(m, c(0, 0, 1))
        if (!identical(dim(m), c(3L, 3L)))
            stop("'affine' must be a 2x2, 2x3, or 3x3 matrix")
    }
    list(a = m[1, 1], b = m[1, 2], d = m[2, 1], e = m[2, 2],
         xoff = m[1, 3], yoff = m[2, 3])
}

#' @export
print.CoordinateTransform <- function(x, ...) {
    cat("<CoordinateTransform: ", x$type, ">\n", sep = "")
    invisible(x)
}
