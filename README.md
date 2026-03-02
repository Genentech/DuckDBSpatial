# DuckDBSpatial

**Optional spatial extension for BiocDuckDB and DuckDBDataFrame**

## Overview

DuckDBSpatial provides 68 `sf`-compatible spatial methods for DuckDB-backed data structures, enabling memory-efficient spatial operations on large datasets. The package includes GeoParquet 1.0 I/O support and seamless integration with DuckDB's native spatial extension.

## Key Features

- **68 sf-compatible methods**: Spatial predicates (`st_intersects`, `st_contains`, `st_within`), unary operations (`st_buffer`, `st_centroid`, `st_convex_hull`), binary set operations (`st_union`, `st_difference`, `st_intersection`), and more
- **Lazy evaluation**: Operations generate SQL pushed down to DuckDB—no data materialization until needed
- **GeoParquet 1.0 support**: `writeGeoParquet()` creates files readable by DuckDB, GeoPandas, GDAL, and other GeoParquet-aware tools
- **DuckDB spatial extension**: Direct integration with DuckDB's native `GEOMETRY` type
- **MultiAssaySpatialExperiment integration**: Required for MASE I/O in BiocDuckDB

## Installation

```r
# From Bioconductor
BiocManager::install("DuckDBSpatial")

# Or from source
devtools::install("DuckDBSpatial")
```

## Quick Start

### GeoParquet I/O

```r
library(DuckDBSpatial)
library(sf)

# Write spatial data as GeoParquet
nc <- st_read(system.file("shape/nc.shp", package="sf"))
writeGeoParquet(nc, "nc.parquet")

# Read in DuckDB with native GEOMETRY type
library(DuckDBDataFrame)
ddb <- DuckDBDataFrame("nc.parquet")
ddb$geometry  # Native GEOMETRY column (lazy)
```

### Lazy Spatial Operations

```r
# Spatial predicates stay lazy
library(sf)

# Create a DuckDBDataFrame with geometry
pts <- st_sf(
    id = 1:1000,
    x = runif(1000, 0, 10),
    y = runif(1000, 0, 10),
    geometry = st_as_sfc(lapply(1:1000, function(i) st_point(c(runif(1, 0, 10), runif(1, 0, 10)))))
)
writeGeoParquet(pts, "points.parquet")
ddb_pts <- DuckDBDataFrame("points.parquet")

# Spatial filter (generates SQL, stays lazy)
bbox <- st_as_sfc("POLYGON((2 2, 8 2, 8 8, 2 8, 2 2))")
inside <- st_intersects(ddb_pts$geometry, bbox)

# Only materialize filtered rows
pts_subset <- ddb_pts[inside, ]
as.data.frame(pts_subset)  # Pulls data from DuckDB
```

### Spatial Methods

All methods operate on `DuckDBTable` (data frames) or `DuckDBColumn` (individual columns):

**Predicates** (return logical vectors):
- `st_intersects`, `st_contains`, `st_within`, `st_covers`, `st_covered_by`
- `st_crosses`, `st_overlaps`, `st_touches`, `st_disjoint`, `st_equals`
- `st_is_within_distance`, `st_contains_properly`, `st_within_properly`

**Unary operations** (geometry → geometry):
- `st_buffer`, `st_centroid`, `st_convex_hull`, `st_envelope`, `st_boundary`
- `st_simplify`, `st_make_valid`, `st_normalize`, `st_reverse`
- `st_line_merge`, `st_node`, `st_point_on_surface`

**Binary operations** (geometry × geometry → geometry):
- `st_intersection`, `st_union`, `st_difference`, `st_sym_difference`
- `st_nearest_points`, `st_shortest_line`

**Measurements**:
- `st_area`, `st_length`, `st_perimeter`, `st_distance`

**Coercion**:
- `st_as_text`, `st_as_binary`, `st_as_sfc`, `st_coordinates`, `st_bbox`

See `?DuckDBTable-spatial` and `?DuckDBColumn-spatial` for complete documentation.

## Integration with BiocDuckDB

DuckDBSpatial is required for MultiAssaySpatialExperiment I/O in BiocDuckDB:

```r
library(BiocDuckDB)
library(MultiAssaySpatialExperiment)
library(DuckDBSpatial)

# Write MASE with spatial layers (shapes written as GeoParquet)
writeParquet(mase_obj, "mase_output")

# Read back with DuckDB-backed spatial layers
mase <- readParquet("mase_output")
spatialShapes(mase)  # DuckDBDataFrame with GEOMETRY columns
```

## GeoParquet Specification

`writeGeoParquet()` conforms to [GeoParquet 1.0.0](https://geoparquet.org/):

- **Encoding**: Well-Known Binary (WKB)
- **Metadata**: Includes geometry types, bounding box, primary column
- **Compatibility**: DuckDB, GeoPandas, GDAL, QGIS, sf

Example metadata:
```json
{
  "version": "1.0.0",
  "primary_column": "geometry",
  "columns": {
    "geometry": {
      "encoding": "WKB",
      "geometry_types": ["Polygon"],
      "bbox": [-84.32, 33.88, -75.46, 36.59]
    }
  }
}
```

## Performance

DuckDBSpatial enables spatial operations on datasets too large for memory:

- **Lazy evaluation**: No data loading until needed
- **Column pruning**: Read only required columns
- **Predicate pushdown**: Spatial filters evaluated in DuckDB
- **Partitioned datasets**: Leverage Arrow partitioning

Example: 1 billion points, 100 GB GeoParquet file—spatial filter runs without loading full dataset.

## Dependencies

**Required** (Depends/Imports):
- **DuckDBDataFrame**: Core DuckDB table/column classes
- **sf**: Spatial features framework (for coercion and compatibility)
- **BiocGenerics**, **S4Vectors**, **dplyr**: Core Bioconductor/data manipulation

**Optional** (Suggests, checked with `requireNamespace()`):
- **nanoparquet**: GeoParquet metadata generation (only needed for `writeGeoParquet()`)
- **jsonlite**: JSON metadata handling (only needed for `writeGeoParquet()`)

The package will error gracefully with installation instructions if optional dependencies are missing when calling `writeGeoParquet()`.

## See Also

- [BiocDuckDB](https://github.com/Genentech/BiocDuckDB) - High-level Bioconductor integration
- [MultiAssaySpatialExperiment](https://github.com/Genentech/MultiAssaySpatialExperiment) - Spatial transcriptomics container
- [DuckDB Spatial Extension](https://duckdb.org/docs/extensions/spatial) - DuckDB spatial documentation
- [GeoParquet](https://geoparquet.org/) - GeoParquet specification

## License

MIT + file LICENSE

## Authors

- Patrick Aboyoun (Genentech, Inc.)
