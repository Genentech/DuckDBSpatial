# DuckDBSpatial 0.99.9

## New features

- `.as_duckdb_table()` (the in-memory materializer backing `st_join`/
  `st_join_table`'s `sf`/`data.frame` support, added in 0.99.8) now also
  accepts a plain WKT-text geometry column, not just `sfc` or raw WKB.
  `MultiAssaySpatialExperiment`'s own idiomatic `DataFrame` shape (see its
  `spatialJoin,DataFrame,DataFrame-method`) stores geometry as WKT
  character strings, the same single-geometry shape `.geom_to_sql()`
  already parses, now generalized to a whole column via `sf::st_as_sfc()`.
  `sf::st_as_sfc()` errors outright on `NA` rather than tolerating it the
  way the `sfg`/WKB paths do, so an `NA` entry is substituted with
  `"GEOMETRYCOLLECTION EMPTY"` first (same never-matches-anything
  semantics as the `NULL`-handling added for the WKB-list case). Added
  regression tests covering a working WKT column, an `NA` entry, and
  genuinely unparseable WKT text.

# DuckDBSpatial 0.99.8

## New features

- Added `st_join_table(x, y, join = st_intersects, sparse = TRUE)`: a sparse
  `(id_x, id_y)` row-pair analogue of the existing `st_join.DuckDBTable`,
  for two lazy `DuckDBTable`/`DuckDBDataFrame` layers. `st_intersects_table()`
  only ever supported testing `x` against a single reference geometry (its
  `y` goes through `.geom_to_sql()`, which has no case for another lazy
  table), so there was no way to get a sparse match table between two
  out-of-core layers short of materializing the full attribute join via
  `st_join()`. `st_join_table()` closes that gap with the same
  `INNER JOIN ... ON <predicate>(x.geometry, y.geometry)` approach as
  `st_join.DuckDBTable`, honoring the `join=` predicate via the same
  `.st_predicate_sql_name()` mapping, just projecting row-pair ids instead
  of full attributes. Internally assigns row ids via the same `__rowid__`
  alias convention `st_intersects_table()` already uses rather than
  `id_x`/`id_y` directly: an independent review before this shipped caught
  that the first draft used `id_x`/`id_y` as the *internal* alias too, so a
  table with its own pre-existing `id_x`/`id_y` data column (plausible,
  since that's exactly this function's own output name) would silently
  collide -- DuckDB auto-renames the duplicate rather than erroring, so the
  final result would silently return that stale data value instead of a
  row index, with no warning. Added a regression test using an injected
  colliding `id_x`/`id_y` column.
- `st_join.DuckDBTable`/`st_join_table` now also accept an in-memory `sf`
  object (or a `data.frame` with an `sfc` or raw-WKB-list geometry column,
  the latter matching the shape `as.data.frame()` on a `DuckDBDataFrame`'s
  own geometry column already produces) on either side, not just lazy
  tables, closing a real gap flagged by a user comparing against
  `duckspatial::ddbs_intersects()`. New internal `.as_duckdb_table()`
  materializes the in-memory side by converting its geometry to WKT
  (`sf::st_as_text()`, the same idiom `.geom_to_sql()` already uses for a
  single geometry) and copying it into a temp DuckDB table via
  `dplyr::copy_to()`, then casting back to native `GEOMETRY` with
  `ST_GeomFromText()`; the temp-table name uses the same PID+counter
  convention `DuckDBArray::.duckdb_matmult_copy()` already established to
  avoid the `dbplyr::unique_table_name()` global-RNG collision class fixed
  in `DuckDBArray` 0.99.9. Chosen over `writeGeoParquet()` + a tempfile
  (the only other working materialization path in this package) since that
  would pull in `nanoparquet` as a hard dependency (currently `Suggests`
  only) and add disk I/O for what's typically the small side of a join;
  CRS is not preserved either way, consistent with `.geom_to_sql()`'s
  existing CRS-less `ST_GeomFromText()` usage, so this isn't a new
  limitation. Also removed `.spatialMatch_sql()`'s dead `else` branch
  (`DuckDBDataFrame-spatial.R`): it attempted the same `DuckDBDataFrame(
  as.data.frame(table))` call that didn't actually work (its only caller,
  `layerSpatialMatch()`, never reaches it with a non-`DuckDBTable` `table`,
  taking a separate in-memory `sf::st_intersects()` path instead), so it
  was unreachable, untested dead code.

## Bug fixes

- Both vignettes, and the `@examples` in `DuckDBColumn-spatial.R`,
  `DuckDBTable-spatial.R`, and `DuckDBDataFrame-spatial.R` (regenerated
  into `DuckDBColumn-spatial.Rd`, `DuckDBTable-spatial.Rd`, and
  `layerSpatialMatch.Rd`), filtered a lazy column with
  `df[which(!is.na(df$type)), ]`. `is.na()`/`!` on a `DuckDBColumn` return
  another lazy `DuckDBColumn` (SQL-pushed), not a base logical vector, so
  `which()` errored with "argument to 'which' is not logical" and broke
  both `R CMD build` and `R CMD check`'s example checks.
  `DuckDBDataFrame`'s row-subset method already accepts a logical
  `DuckDBColumn` directly as `i`, pushing it down as a SQL `WHERE` filter,
  so `which()` was unnecessary; fixed to `df[!is.na(df$type), ]`
  everywhere.

# DuckDBSpatial 0.99.6

## Documentation

- Fixed two instances of `` `Type`s `` in the vignettes (a plural "s"
  immediately after a closing code-span backtick, e.g. "lazy
  `` `DuckDBColumn`s ``"), which renders with a visible gap that reads
  like a separate word "s". Reworded to "lazy `DuckDBColumn` objects" /
  "lazy `DuckDBDataFrame` objects" instead of moving the "s" inside the
  backticks, since neither class name is actually plural.

# DuckDBSpatial 0.99.5

## Bug fixes

- `writeGeoParquet()` wrote the GeoParquet `crs` metadata field as a raw
  WKT2 string instead of the PROJJSON object the GeoParquet 1.0.0 spec
  requires, which made DuckDB itself refuse to read back any file written
  with a CRS set (`"Geoparquet column 'geometry' has invalid CRS"`). Fixed
  via `wk::wk_crs_projjson()` (`wk` is already a transitive dependency via
  `sf`); added `wk` to `Imports`. Only surfaced once the `writeGeoParquet()`
  example was made to actually run (see Documentation below).

## Documentation

- Added explanatory prose and links to `sf` documentation throughout both
  vignettes, for readers unfamiliar with `sf`: geometry columns/WKB,
  `st_geometry_type()`/`st_area()`, `st_centroid()`/WKT via
  `st_as_text()`, the `st_point()`/`st_sfc()` query-geometry idiom, WKT
  string literals passed to `st_as_sfc()`, and `st_sf()`.
- Added runnable `@examples` to `asCoordinateTransform()`, `st_affine()`,
  and `writeGeoParquet()` (previously undocumented, or `\dontrun`-only).
- `writeGeoParquet.Rd`'s example switched from `\dontrun` to `\donttest`.
- Added `inst/scripts/make-extdata-spatial.R`, documenting the bundled
  `inst/extdata/spatial` fixtures as hand-authored synthetic data (the
  canonical OGC/Wikipedia WKT example shapes for each geometry type, plus
  one `EMPTY` and one `NULL` geometry per type for edge-case testing),
  licensed under this package's own MIT license.

## Internal changes

- Removed `.st_point_sql()`, dead code superseded by the exported
  `st_point_sql()`.

# DuckDBSpatial 0.99.4

## Documentation

- Replaced em dashes with commas or colons in the vignettes.

# DuckDBSpatial 0.99.3

## Documentation

- Briefly define "predicate" at its first use in the user and design vignettes
  (a true/false test of how two geometries relate), addressing the reviewer's
  note that the term may be unfamiliar to some readers.

# DuckDBSpatial 0.99.2

## Documentation

- Added `URL` and `BugReports` fields to DESCRIPTION.
- Added a package-level man page (`?DuckDBSpatial`).
- Removed the redundant `library(BiocStyle)` call from the vignettes (the
  `BiocStyle::html_document` output and `::`-qualified helpers apply the style
  without loading the package). Applied suite-wide for consistency with the
  Bioconductor review of DuckDBDataFrame.

# DuckDBSpatial 0.9.12

## New features

- 3-D coord-indexed points. `spatialSortPoints()` and `writeSpatialPointsParquet()`
  gain a `z_col` argument and cluster on a genuine 3-D Morton curve
  `zorder(c(x, y, z))` (the shared generator is already N-D — not a 2-D curve with
  `z` appended). The writer **auto-detects a `"z"` column** by default (so 3-D
  points cluster in 3-D automatically, matching scibis's writer and keeping the
  points Parquet byte-identical across the two stacks); `z_col = NA` forces 2-D, a
  name overrides. `layerBboxRange()` gains `zmin`/`zmax` (and `z_col`): when both
  are supplied it ANDs a `z BETWEEN` term that pushes into the Parquet scan too, so
  a 3-D layout prunes row groups on all three axes. Still a pure row permutation +
  plain range predicate — no column added, no secondary index.

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
