# DuckDBSpatial 0.9.9

## Bug fixes

- Loading the package now installs and loads the DuckDB `spatial` extension on the
  shared `DuckDBDataFrame` connection (via `DuckDBDataFrame::loadExtension()` in
  `.onLoad`). Every DuckDBSpatial operation compiles to `ST_*` SQL resolved
  against that connection, and DuckDB's autoload did not reliably install the
  extension (e.g. in a fresh `R CMD check` environment), so `ST_*` calls failed
  with "… is not in the catalog, but it exists in the spatial extension". The
  install is best-effort (`optional = TRUE`) and **silent**: where the extension
  cannot be obtained (offline / restricted network / TLS interception) package
  loading still succeeds without emitting any warning or message, and spatial
  operations error only when actually used (with DuckDB's own guidance). To
  pre-seed in a restricted environment, place a version-matched
  `spatial.duckdb_extension` under `DUCKDB_EXTENSION_DIRECTORY`, or point
  `MODL_DUCKDB_EXTENSION_REPOSITORY` at a reachable mirror.
- `st_join()` on a `DuckDBTable` now honors its `join` predicate argument instead
  of always using `ST_Intersects`. The predicate function is mapped to the
  matching DuckDB `ST_*` function (`st_within` -> `ST_Within`, `st_contains` ->
  `ST_Contains`, etc.); an unrecognized predicate errors rather than silently
  falling back to intersects.
- `writeGeoParquet()` now records the geometry column's **CRS** (WKT2) in the
  GeoParquet metadata. Previously the `crs` key was omitted, so readers fell back
  to the spec default of OGC:CRS84 (WGS84 lon/lat) and silently mislabeled
  projected data.
- `writeGeoParquet()` now **omits** the `bbox` metadata when the extent is unknown
  (empty / non-finite geometry), per the GeoParquet spec, instead of writing a
  false `[0, 0, 0, 0]` extent at the origin that would mis-prune bbox-based
  spatial filters.

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
