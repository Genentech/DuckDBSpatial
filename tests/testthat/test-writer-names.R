# The writers refuse columns whose names DuckDB would take for one another.

test_that("writeGeoParquet refuses columns whose names are the same up to case", {
    skip_if_not_installed("nanoparquet")
    pts <- sf::st_as_sf(data.frame(Sex = c("M", "F"), sex = c("male", "female"),
                                   x = c(0, 1), y = c(0, 1)),
                        coords = c("x", "y"), crs = 4326)
    out <- tempfile(fileext = ".parquet")
    on.exit(unlink(out))
    expect_error(writeGeoParquet(pts, out), "'Sex', 'sex'")
    expect_false(file.exists(out))
    ok <- pts
    ok$sex <- NULL
    writeGeoParquet(ok, out)
    expect_true(file.exists(out))
})

test_that("writeSpatialPointsParquet refuses columns whose names are the same up to case", {
    d <- data.frame(x = c(0, 1, 2), y = c(2, 1, 0), Gene = c("a", "b", "c"),
                    gene = c("A", "B", "C"))
    out <- tempfile(fileext = ".parquet")
    on.exit(unlink(out))
    expect_error(writeSpatialPointsParquet(d, out), "'Gene', 'gene'")
    expect_false(file.exists(out))
    d$gene <- NULL
    writeSpatialPointsParquet(d, out)
    expect_true(file.exists(out))
})
