# DuckDBSpatial 0.9.8

## Documentation

- Made the vignettes resilient to a missing DuckDB `spatial` extension: a setup
  probe (`.spatial_ok`) gates the live spatial chunks, so `R CMD build` and the
  modl-docs render succeed (with a short note) where the extension cannot be
  obtained (e.g. an offline developer machine or a restricted CI network), and
  render the examples for real where it is available.

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
