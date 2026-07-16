# DuckDBSpatial

*sf-compatible spatial operations on DuckDB-backed data — out-of-core geometries, stored as GeoParquet.*

## Overview

`DuckDBSpatial` extends [DuckDBDataFrame](https://bioconductor.org/packages/DuckDBDataFrame)
with [sf](https://r-spatial.github.io/sf/)-compatible spatial methods that run on
DuckDB-backed columns and tables, powered by DuckDB's native spatial extension. It
exposes much of the `sf` spatial vocabulary --- predicates (`st_intersects`,
`st_within`, `st_contains`), measurements (`st_area`, `st_length`), and geometry
transforms (`st_centroid`, `st_buffer`, `st_union`) --- but the geometries stay
**on disk** in columnar Parquet and operations are recorded as lazy SQL, so you
can filter and transform spatial layers **without loading them into memory**.

It reads and writes **GeoParquet 1.0**, so the same files interoperate with
GeoPandas, GDAL, QGIS, and DuckDB. Within the **BiocDuckDB** suite it is the
spatial layer: it serves `GEOMETRY` columns for `DuckDBDataFrame` and underpins the
`MultiAssaySpatialExperiment` on-disk format in `BiocDuckDB`.

## Installation

```r
# once available from Bioconductor:
if (!require("BiocManager")) install.packages("BiocManager")
BiocManager::install("DuckDBSpatial")
```

The DuckDB spatial extension is fetched and cached automatically the first time a
spatial operation runs.

## Quick start

```r
library(DuckDBSpatial)
library(sf)

# open a GeoParquet layer as a lazy DuckDBDataFrame
spatial_path <- system.file("extdata", "spatial", package = "DuckDBSpatial")
df <- DuckDBDataFrame(spatial_path)

geom <- df[["geometry"]]
head(st_area(geom))                       # lazy; ST_Area pushed into DuckDB

query_pt <- st_sfc(st_point(c(30, 10)))
st_filter(df, query_pt)                    # keep rows intersecting the query
```

For plain `x`/`y` coordinate columns (e.g. transcript centroids), the `layer*`
helpers run predicates without a geometry column:

```r
layerSubsetByGeometry(pts, poly, coords = c("x", "y"))
```

## GeoParquet I/O

```r
ddb <- readGeoParquet("layer.parquet")     # lazy DuckDBDataFrame w/ GEOMETRY column
writeGeoParquet(sf_object, "out.parquet")  # GeoParquet 1.0 (WKB + metadata)
```

`writeGeoParquet()` conforms to [GeoParquet 1.0.0](https://geoparquet.org/) (WKB
encoding, geometry-type and bounding-box metadata) and needs the suggested
`nanoparquet` and `jsonlite` packages.

## When to use DuckDBSpatial

A good fit when the spatial layer is **larger than memory**, when the workload is
**filter-heavy** (selecting features by region or predicate before the expensive
step), or when the data lives on disk as **GeoParquet** shared with other spatial
tooling. An in-memory `sf` object remains preferable for small layers and
interactive geometry editing.

## Documentation

- **Introduction to DuckDBSpatial** — motivation, lazy spatial columns, predicates
  and filtering, layer engines, and GeoParquet I/O (`vignettes/DuckDBSpatial.Rmd`).
- **Design and extension of DuckDBSpatial** — the `sf`-generic to DuckDB spatial
  SQL translation, the two dispatch paths, GeoParquet 1.0 I/O, and the BiocDuckDB
  integration, for developers (`vignettes/DuckDBSpatial-design.Rmd`).

## License

MIT License. Copyright Genentech, Inc., 2026.
