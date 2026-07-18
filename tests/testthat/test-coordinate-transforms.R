# The coordinate-transform algebra (coordinate-transforms.R): constructors, the
# affine-matrix lowering, application, inversion, and the OME-NGFF RFC-5 conformance
# suite (vendored point-level vectors, resolved through the CT graph).
# library(testthat); library(DuckDBSpatial); source("test-coordinate-transforms.R")

test_that("constructors build classed transforms and coerce from dicts", {
    expect_s3_class(ctScale(c(2, 3)), "CoordinateTransform")
    expect_identical(ctScale(c(2, 3))$type, "scale")
    # a dict (as read from JSON) round-trips through asCoordinateTransform
    tf <- asCoordinateTransform(list(type = "translation", translation = c(1, 2)))
    expect_identical(tf$type, "translation")
    # recursion into sequence
    sq <- asCoordinateTransform(list(type = "sequence", transformations = list(
        list(type = "scale", scale = c(2, 2)), list(type = "identity"))))
    expect_length(sq$transformations, 2L)
    expect_s3_class(sq$transformations[[1]], "CoordinateTransform")
})

test_that("ctApply matches hand-computed affine outputs", {
    ax <- c("y", "x")
    expect_equal(ctApply(ctScale(c(10, 20)), rbind(c(1, 2)), ax, ax),
                 rbind(c(10, 40)))
    expect_equal(ctApply(ctTranslation(c(10, 20)), rbind(c(1, 2)), ax, ax),
                 rbind(c(11, 22)))
    # rotate 90 about origin: (x,y)->(-y,x) in (y,x) order stays consistent
    r <- ctRotation(rbind(c(0, -1), c(1, 0)))
    expect_equal(ctApply(r, rbind(c(1, 0)), ax, ax), rbind(c(0, 1)))
})

test_that("ctInvert round-trips for the invertible types", {
    ax <- c("y", "x")
    pts <- rbind(c(-1, 2), c(3, 5))
    for (tf in list(ctScale(c(2, 5)), ctTranslation(c(7, -3)),
                    ctRotation(rbind(c(0, -1), c(1, 0))),
                    ctAffine(rbind(c(2, 1, 3), c(0, 4, 5))),
                    ctMapAxis(c(1, 0)),
                    ctSequence(list(ctScale(c(2, 3)), ctTranslation(c(1, 1)))))) {
        fwd <- ctApply(tf, pts, ax, ax)
        back <- ctApply(ctInvert(tf), fwd, ax, ax)
        expect_equal(back, pts, tolerance = 1e-9, info = tf$type)
    }
})

test_that("a dimensionality-changing affine cannot be inverted", {
    expect_error(ctInvert(ctAffine(rbind(c(2, 2, 2, 0), c(3, 3, 3, 0)))),
                 "dimensionality-changing")
})

test_that("RFC-5 conformance vectors pass (algebra + CT graph)", {
    skip_if_not_installed("jsonlite")
    fx <- jsonlite::fromJSON(test_path("fixtures", "rfc5-conformance.json"),
                             simplifyDataFrame = FALSE, simplifyMatrix = TRUE)
    for (case in fx$cases) {
        g <- ctGraph(case$edges, systems = case$coordinate_systems)
        if (isTRUE(case$should_error)) {
            expect_error(ctPath(g, case$source$name, case$target$name),
                         info = case$name)
            next
        }
        got <- ctBetween(g, case$source$name, case$target$name,
                         case$source$coordinates)
        exp <- case$target$coordinates
        within <- abs(got - exp) <=
            case$atol + case$rtol * pmax(abs(got), abs(exp))
        expect_true(all(within), info = case$name)
    }
})
