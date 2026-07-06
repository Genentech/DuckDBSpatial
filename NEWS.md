# DuckDBSpatial 0.9.7

## Documentation

- Restructured the vignettes into a user-first set, replacing the single
  *Overview of the DuckDBSpatial Package*:
  - *Introduction to DuckDBSpatial* --- motivation, lazy spatial columns,
    predicates and `st_filter()`, the layer-level engines for coordinate columns,
    and GeoParquet I/O.
  - *Design and extension of DuckDBSpatial* --- how the `sf` generics translate to
    DuckDB spatial SQL, the two dispatch paths (geometry columns vs coordinate
    columns), GeoParquet 1.0 I/O, and the BiocDuckDB / MultiAssaySpatialExperiment
    integration, for developers.
- Rewrote the README.
