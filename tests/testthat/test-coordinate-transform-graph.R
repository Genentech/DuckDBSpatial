# The coordinate-transform graph (coordinate-transform-graph.R): building the graph
# with auto-added inverse edges, shortest-path resolution, ambiguity / no-path /
# unknown-system errors, and end-to-end ctBetween.
# library(testthat); library(DuckDBSpatial); source("test-coordinate-transform-graph.R")

.two_d <- function(...) list(...)

test_that("coordinateSystem records axis names", {
    cs <- coordinateSystem("global", c("y", "x"))
    expect_s3_class(cs, "CoordinateSystem")
    expect_identical(cs$axes, c("y", "x"))
})

test_that("ctGraph adds a reverse edge and ctPath traverses it", {
    g <- ctGraph(
        list(list(input = "a", output = "b", transform = ctScale(c(2, 4)))),
        systems = list(a = c("y", "x"), b = c("y", "x")))
    # forward: the edge transform
    fwd <- ctPath(g, "a", "b")
    expect_equal(ctApply(fwd, rbind(c(1, 1)), c("y", "x"), c("y", "x")),
                 rbind(c(2, 4)))
    # backward: the auto-added inverse edge (1/scale)
    back <- ctPath(g, "b", "a")
    expect_equal(ctApply(back, rbind(c(2, 4)), c("y", "x"), c("y", "x")),
                 rbind(c(1, 1)))
})

test_that("ctPath returns identity for the same system", {
    g <- ctGraph(list(list(input = "a", output = "b", transform = ctIdentity())),
                 systems = list(a = c("y", "x"), b = c("y", "x")))
    expect_identical(ctPath(g, "a", "a")$type, "identity")
})

test_that("ctPath errors on unknown systems and missing paths", {
    g <- ctGraph(list(list(input = "a", output = "b", transform = ctIdentity())),
                 systems = list(a = c("y", "x"), b = c("y", "x"), c = c("y", "x")))
    expect_error(ctPath(g, "nope", "b"), "unknown coordinate system")
    expect_error(ctPath(g, "a", "nope"), "unknown coordinate system")
    expect_error(ctPath(g, "a", "c"), "no coordinate-transform path")  # c isolated
})

test_that("ctPath errors when two shortest paths tie (ambiguous)", {
    # diamond: a->m1->b and a->m2->b are both length 2
    g <- ctGraph(list(
        list(input = "a", output = "m1", transform = ctIdentity()),
        list(input = "a", output = "m2", transform = ctIdentity()),
        list(input = "m1", output = "b", transform = ctIdentity()),
        list(input = "m2", output = "b", transform = ctIdentity())))
    expect_error(ctPath(g, "a", "b"), "ambiguous")
})

test_that("ctBetween composes a multi-hop path", {
    g <- ctGraph(list(
        list(input = "a", output = "m", transform = ctScale(c(2, 2))),
        list(input = "m", output = "b", transform = ctTranslation(c(10, 20)))),
        systems = list(a = c("y", "x"), m = c("y", "x"), b = c("y", "x")))
    # scale first, then translate: (1,1) -> (2,2) -> (12,22)
    expect_equal(ctBetween(g, "a", "b", rbind(c(1, 1))), rbind(c(12, 22)))
})
