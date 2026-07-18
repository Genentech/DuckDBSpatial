# DuckDBSpatial 0.9.11

## New features

- Coordinate-transform operators + a coordinate-transform graph, closing the
  largest OME-NGFF / spatialdataR parity gap (previously only
  `st_flip_coordinates` + CRS metadata).
  - A transform algebra (`coordinate-transforms.R`): `ctIdentity()`, `ctScale()`,
    `ctTranslation()`, `ctRotation()`, `ctAffine()`, `ctSequence()`, `ctMapAxis()`,
    `ctByDimension()`, `ctBijection()` build the OME-NGFF RFC-5 affine family;
    `ctAffineMatrix()` lowers any transform to a homogeneous affine matrix,
    `ctApply()` applies it to a matrix of points, and `ctInvert()` inverts it. The
    full matrix-expressible RFC-5 conformance suite (incl. 3-D, dimensionality
    change, and multi-hop paths) is vendored as point-level vectors and passes.
  - A coordinate-transform graph (`coordinate-transform-graph.R`):
    `coordinateSystem()`, `ctGraph()` (auto-adds inverse edges for invertible
    transforms), `ctPath()` (shortest-path resolution; errors on unknown systems,
    no path, or an ambiguous tie), and `ctBetween()` (resolve + apply). Base R
    only -- no graph/RBGL dependency.
  - DuckDB data application: `st_affine()` transforms a `DuckDBTable` /
    `DuckDBColumn` geometry column via `ST_Affine`, and `transformLayer()` applies
    a (2-D) coordinate transform to a lazy spatial layer -- point `x`/`y` columns by
    arithmetic, a geometry column by `ST_Affine`. Displacement / coordinate-field
    ("by path") transforms are out of scope.
- Coord-indexed points layout for fast viewport queries (`points-coord-index.R`).
  A viewport query is a bounding-box range predicate on `(x, y)`. Sorting points
  along a space-filling curve before writing gives each Parquet row group a tight
  `x`/`y` zonemap, so DuckDB prunes the groups outside the box and reads only the
  points the query needs.
  - `spatialSortPoints()` reorders a points table by the Morton (Z-order) code of
    its `(x, y)`: a pure row permutation that clusters spatial neighbours and
    round-trips identically.
  - `writeSpatialPointsParquet()` sorts (by default) and writes a points Parquet
    with a bounded `ROW_GROUP_SIZE`, so there are row groups to prune;
    `spatial_sort = FALSE` writes the acquisition-order baseline.
  - `layerBboxRange()` subsets a `DuckDBDataFrame` point layer to a bounding box
    with a plain `x`/`y BETWEEN` predicate that pushes into the Parquet scan and
    prunes, unlike `layerSubsetByBbox()`, which builds a per-row `ST_Point` and
    tests `ST_Intersects`. Returns a lazy `DuckDBDataFrame`.

  This is the 2-D spatial case of DuckDBDataFrame's general `cluster_by` directive:
  `spatialSortPoints()` delegates to `DuckDBDataFrame::clusterSort()` with
  `DuckDBDataFrame::zorder()`, so the stack keeps a single Morton generator.

# DuckDBSpatial 0.9.10

## Changes

- Relicensed under the MIT License.

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
  `BIOCDUCKDB_EXTENSION_REPOSITORY` at a reachable mirror.
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
