## Regenerates inst/extdata/spatial/, the small Hive-partitioned ("type=...")
## GeoParquet dataset bundled with DuckDBSpatial and used throughout its
## vignettes, man page examples, and tests (via
## system.file("extdata", "spatial", package = "DuckDBSpatial")).
##
## Source and license: this is entirely synthetic, hand-authored data, not
## derived from any external dataset. Each geometry type gets one or two
## illustrative shapes -- deliberately the canonical example geometries used
## throughout the OGC Simple Feature Access literature and Wikipedia's
## "Well-known text representation of geometry" article
## (https://en.wikipedia.org/wiki/Well-known_text_representation_of_geometry),
## chosen because they're widely recognized reference shapes rather than
## anything arbitrary -- plus one EMPTY geometry and one NULL (missing)
## geometry per type, to exercise those edge cases in tests and examples.
## Licensed under this package's own MIT license (see LICENSE), like the
## rest of the package source.
##
## Run from the package root to regenerate inst/extdata/spatial/ in place:
##   Rscript inst/scripts/make-extdata-spatial.R

library(arrow)
library(sf)

wkt_by_type <- list(
    point = c(
        "POINT (30 10)",
        "POINT EMPTY",
        NA_character_,
        "POINT (40 40)"
    ),
    multipoint = c(
        "MULTIPOINT ((30 10))",
        "MULTIPOINT ((10 40), (40 30), (20 20), (30 10))",
        "MULTIPOINT EMPTY",
        NA_character_
    ),
    linestring = c(
        "LINESTRING (30 10, 10 30, 40 40)",
        "LINESTRING EMPTY",
        NA_character_
    ),
    multilinestring = c(
        "MULTILINESTRING ((30 10, 10 30, 40 40))",
        "MULTILINESTRING ((10 10, 20 20, 10 40), (40 40, 30 30, 40 20, 30 10))",
        "MULTILINESTRING EMPTY",
        NA_character_
    ),
    polygon = c(
        "POLYGON ((30 10, 40 40, 20 40, 10 20, 30 10))",
        "POLYGON ((35 10, 45 45, 15 40, 10 20, 35 10), (20 30, 35 35, 30 20, 20 30))",
        "POLYGON EMPTY",
        NA_character_
    ),
    multipolygon = c(
        "MULTIPOLYGON (((30 10, 40 40, 20 40, 10 20, 30 10)))",
        "MULTIPOLYGON (((30 20, 45 40, 10 40, 30 20)), ((15 5, 40 10, 10 20, 5 10, 15 5)))",
        "MULTIPOLYGON (((40 40, 20 45, 45 30, 40 40)), ((20 35, 10 30, 10 10, 30 5, 45 20, 20 35), (30 20, 20 15, 20 25, 30 20)))",
        "MULTIPOLYGON EMPTY",
        NA_character_
    )
)

## One row per shape: an integer position ("col"), the geometry as raw WKB
## (NULL for the missing-geometry row), and the partitioning "type" column.
rows <- do.call(rbind, lapply(names(wkt_by_type), function(type) {
    wkt <- wkt_by_type[[type]]
    geometry <- lapply(wkt, function(w) {
        if (is.na(w)) return(NULL)
        st_as_binary(st_as_sfc(w)[[1L]])
    })
    data.frame(col = seq_along(wkt) - 1L, type = type,
               geometry = I(geometry))
}))

out_dir <- file.path("inst", "extdata", "spatial")
unlink(out_dir, recursive = TRUE)
write_dataset(rows, out_dir, partitioning = "type", format = "parquet")
