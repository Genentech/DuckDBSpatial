# Spatial dataset
spatial_wkt <- c("LINESTRING (30 10, 10 30, 40 40)",
                 "LINESTRING EMPTY",
                 NA_character_,
                 "MULTILINESTRING ((30 10, 10 30, 40 40))",
                 "MULTILINESTRING ((10 10, 20 20, 10 40), (40 40, 30 30, 40 20, 30 10))",
                 "MULTILINESTRING EMPTY",
                 NA_character_,
                 "MULTIPOINT (30 10)",
                 "MULTIPOINT (10 40, 40 30, 20 20, 30 10)",
                 "MULTIPOINT EMPTY",
                 NA_character_,
                 "MULTIPOLYGON (((30 10, 40 40, 20 40, 10 20, 30 10)))",
                 "MULTIPOLYGON (((30 20, 45 40, 10 40, 30 20)), ((15 5, 40 10, 10 20, 5 10, 15 5)))",
                 "MULTIPOLYGON (((40 40, 20 45, 45 30, 40 40)), ((20 35, 10 30, 10 10, 30 5, 45 20, 20 35), (30 20, 20 15, 20 25, 30 20)))",
                 "MULTIPOLYGON EMPTY",
                 NA_character_,
                 "POINT (30 10)",
                 "POINT EMPTY",
                 NA_character_,
                 "POINT (40 40)",
                 "POLYGON ((30 10, 40 40, 20 40, 10 20, 30 10))",
                 "POLYGON ((35 10, 45 45, 15 40, 10 20, 35 10), (20 30, 35 35, 30 20, 20 30))",
                 "POLYGON EMPTY",
                 NA_character_)
spatial_path <- system.file("extdata", "spatial", package = "DuckDBSpatial")


# Helper functions
checkDuckDBColumn <- function(object, expected) {
    expect_true(validObject(object))
    expect_s4_class(object, "DuckDBColumn")
    expect_true(length(capture.output(show(object))) > 0L)
    expect_identical(dbconn(object), acquireDuckDBConn())
    expect_s3_class(tblconn(object), "tbl_duckdb_connection")
    expect_identical(length(object), length(expected))
    if (nkey(object@table) == 0L) {
        object <- as.vector(object)
        expect_identical(length(object), length(expected))
    } else {
        expect_identical(names(object), names(expected))
        expect_equal(as.vector(object), expected)
        expect_equal(realize(object), expected)
    }
}
