#' Spatial operations on DuckDBTable objects
#'
#' @description
#' Spatial operations on DuckDBTable objects.
#'
#' @section Geometry Creation:
#' In the code snippets below, \code{x} is a DuckDBTable object.
#' \describe{
#'   \item{\code{st_as_sfc(x, ..., crs = NA_integer_, GeoJSON = FALSE, WKB = FALSE)}:}{
#'     Parses WKT (default), GeoJSON, or WKB into GEOMETRY via
#'     \code{ST_GeomFromText}, \code{ST_GeomFromGeoJSON}, or
#'     \code{ST_GeomFromWKB}.
#'   }
#' }
#'
#' @section Geometry Coercion:
#' \describe{
#'   \item{\code{st_as_binary(x, hex = FALSE)}:}{
#'     Converts to WKB (\code{ST_AsWKB}) or hex-encoded WKB
#'     (\code{ST_AsHEXWKB}).
#'   }
#'   \item{\code{st_as_text(x, geojson = FALSE)}:}{
#'     Converts to WKT (\code{ST_AsText}) or GeoJSON
#'     (\code{ST_AsGeoJSON}).
#'   }
#' }
#'
#' @section Geometry Accessors:
#' Scalar properties of each geometry.
#' \describe{
#'   \item{\code{st_dimension(x)}:}{
#'     Topological dimension: 0 (point), 1 (line), or 2 (polygon).
#'   }
#'   \item{\code{st_end_point(x)}:}{
#'     End point of a linestring.
#'   }
#'   \item{\code{st_geometry_type(x)}:}{
#'     Geometry type name as character (e.g. \code{"POINT"},
#'     \code{"POLYGON"}).
#'   }
#'   \item{\code{st_is_closed(x)}:}{
#'     Logical: is the linestring closed (first point = last point)?
#'   }
#'   \item{\code{st_is_empty(x)}:}{
#'     Logical: is the geometry empty?
#'   }
#'   \item{\code{st_is_ring(x)}:}{
#'     Logical: is the linestring both closed and simple?
#'   }
#'   \item{\code{st_is_simple(x)}:}{
#'     Logical: is the geometry simple (e.g. not self-intersecting)?
#'   }
#'   \item{\code{st_is_valid(x)}:}{
#'     Logical: is the geometry valid?
#'   }
#'   \item{\code{st_num_geometries(x)}:}{
#'     Number of geometries in a geometry collection.
#'   }
#'   \item{\code{st_num_interior_rings(x)}:}{
#'     Number of interior rings in a polygon.
#'   }
#'   \item{\code{st_num_points(x)}:}{
#'     Number of points within a geometry.
#'   }
#'   \item{\code{st_start_point(x)}:}{
#'     Start point of a linestring.
#'   }
#' }
#'
#' @section Measurement:
#' \describe{
#'   \item{\code{st_area(x)}:}{
#'     Area of each geometry.
#'   }
#'   \item{\code{st_distance(x, y)}:}{
#'     Euclidean distance from each geometry in \code{x} to \code{y}.
#'     Argument \code{y} accepts WKT character, \code{sfg}, \code{sfc}
#'     (length 1), or \code{call}.
#'   }
#'   \item{\code{st_length(x)}:}{
#'     Length of linestring or multilinestring; zero for points and polygons.
#'   }
#'   \item{\code{st_perimeter(x)}:}{
#'     Perimeter of polygon or multipolygon; zero for points and lines.
#'   }
#' }
#'
#' @section Unary Geometry Operations:
#' Each returns a DuckDBTable with a transformed geometry per row.
#' \describe{
#'   \item{\code{st_boundary(x)}:}{
#'     Geometry boundary.
#'   }
#'   \item{\code{st_buffer(x, dist)}:}{
#'     Buffer by \code{dist} units.
#'   }
#'   \item{\code{st_build_area(x)}:}{
#'     Build area of a geometry.
#'   }
#'   \item{\code{st_centroid(x)}:}{
#'     Centroid point.
#'   }
#'   \item{\code{st_collection_extract(x, type = c("POLYGON", "POINT", "LINESTRING"))}:}{
#'     Extract geometries of given type from collections.
#'   }
#'   \item{\code{st_concave_hull(x, ratio, allow_holes = FALSE)}:}{
#'     Concave hull; \code{ratio} in [0,1] (1 = convex).
#'   }
#'   \item{\code{st_convex_hull(x)}:}{
#'     Convex hull polygon.
#'   }
#'   \item{\code{st_envelope(x)}:}{
#'     Axis-aligned bounding-box polygon.
#'   }
#'   \item{\code{st_exterior_ring(x)}:}{
#'     Exterior ring of a polygon.
#'   }
#'   \item{\code{st_flip_coordinates(x)}:}{
#'     Flip the coordinates of a geometry.
#'   }
#'   \item{\code{st_inscribed_circle(x, dTolerance, ..., nQuadSegs = 30)}:}{
#'     Maximum inscribed circle of polygon; \code{dTolerance} required.
#'   }
#'   \item{\code{st_line_interpolate(line, dist, normalized = FALSE)}:}{
#'     Point at distance \code{dist} along \code{line}; when
#'     \code{normalized = FALSE}, \code{dist} is in line units; when
#'     \code{TRUE}, \code{dist} is fraction in [0,1].
#'   }
#'   \item{\code{st_line_merge(x, directed = FALSE)}:}{
#'     Merge connected line segments.
#'   }
#'   \item{\code{st_line_project(line, point, normalized = FALSE)}:}{
#'     Distance or fraction along \code{line} to nearest point to
#'     \code{point}; \code{normalized} controls units vs fraction.
#'   }
#'   \item{\code{st_line_substring(line, start, end)}:}{
#'     Substring of \code{line} between \code{start} and \code{end} fractions.
#'   }
#'   \item{\code{st_make_valid(x)}:}{
#'     Repair invalid geometry.
#'   }
#'   \item{\code{st_minimum_rotated_rectangle(x)}:}{
#'     Minimum-area rotated bounding rectangle.
#'   }
#'   \item{\code{st_node(x)}:}{
#'     Add nodes at intersection points (noding) for line geometry.
#'   }
#'   \item{\code{st_normalize(x)}:}{
#'     Normalise vertex order.
#'   }
#'   \item{\code{st_point_on_surface(x)}:}{
#'     A point guaranteed to lie on the surface.
#'   }
#'   \item{\code{st_reduce_precision(x, precision)}:}{
#'     Reduce precision of a geometry.
#'   }
#'   \item{\code{st_remove_repeated_points(x)}:}{
#'     Remove repeated points from a geometry.
#'   }
#'   \item{\code{st_reverse(x)}:}{
#'     Reverse vertex order.
#'   }
#'   \item{\code{st_simplify(x, preserveTopology = FALSE, dTolerance = 0.0)}:}{
#'     Simplify geometry.  Uses \code{ST_SimplifyPreserveTopology} when
#'     \code{preserveTopology = TRUE}.
#'   }
#'   \item{\code{st_voronoi(x)}:}{
#'     Voronoi diagram of points (DuckDB: no envelope/tolerance).
#'   }
#' }
#'
#' @section Binary Spatial Predicates:
#' Each takes a second geometry argument \code{y} and returns a BOOLEAN
#' DuckDBTable.  Argument \code{y} accepts a WKT character string, an
#' \code{sfc}/\code{sfg} object from \pkg{sf}, or a \code{call} (raw SQL
#' expression).  Results are usable for row filtering via
#' \code{extractROWS} or \code{subset}.
#' \describe{
#'   \item{\code{st_contains(x, y)}:}{
#'     Does \code{x} contain \code{y}?
#'   }
#'   \item{\code{st_contains_properly(x, y)}:}{
#'     Does \code{x} properly contain \code{y} (boundary excluded)?
#'   }
#'   \item{\code{st_covered_by(x, y)}:}{
#'     Is \code{x} covered by \code{y}?
#'   }
#'   \item{\code{st_covers(x, y)}:}{
#'     Does \code{x} cover \code{y}?
#'   }
#'   \item{\code{st_crosses(x, y)}:}{
#'     Does \code{x} cross \code{y}?
#'   }
#'   \item{\code{st_disjoint(x, y)}:}{
#'     Are \code{x} and \code{y} disjoint?
#'   }
#'   \item{\code{st_equals(x, y)}:}{
#'     Are \code{x} and \code{y} geometrically equal?
#'   }
#'   \item{\code{st_intersects(x, y)}:}{
#'     Do \code{x} and \code{y} intersect?
#'   }
#'   \item{\code{st_is_within_distance(x, y, dist)}:}{
#'     Is \code{x} within \code{dist} of \code{y}?
#'   }
#'   \item{\code{st_overlaps(x, y)}:}{
#'     Does \code{x} overlap \code{y}?
#'   }
#'   \item{\code{st_touches(x, y)}:}{
#'     Does \code{x} touch \code{y}?
#'   }
#'   \item{\code{st_within(x, y)}:}{
#'     Is \code{x} within \code{y}?
#'   }
#'   \item{\code{st_within_properly(x, y)}:}{
#'     Is \code{x} properly within \code{y} (boundary excluded)?
#'   }
#' }
#'
#' @section Binary Set Operations:
#' Each takes a second geometry argument \code{y} (same accepted types as
#' predicates) and returns a GEOMETRY DuckDBTable.
#' \describe{
#'   \item{\code{st_difference(x, y)}:}{
#'     Portion of \code{x} that does not intersect \code{y}.
#'   }
#'   \item{\code{st_intersection(x, y)}:}{
#'     Portion of \code{x} that intersects \code{y}.
#'   }
#'   \item{\code{st_nearest_points(x, y)}:}{
#'     Shortest line between \code{x} and \code{y} (DuckDB \code{ST_ShortestLine}).
#'   }
#'   \item{\code{st_union(x, y)}:}{
#'     Union of \code{x} and \code{y}.  Binary form only at the
#'     DuckDBTable level; aggregate union is available on DuckDBColumn.
#'   }
#' }
#'
#' @section Aggregate Operations:
#' \describe{
#'   \item{\code{st_collect(x)}:}{
#'     Combine all geometries into a single collection.
#'   }
#'   \item{\code{st_union_agg(x, by = NULL)}:}{
#'     Aggregate union over a geometry column via \code{ST_Union_Agg}.
#'   }
#'   \item{\code{st_collect_agg(x, by = NULL)}:}{
#'     Aggregate collect over a geometry column via \code{ST_Collect}.
#'   }
#'   \item{\code{st_envelope_agg(x, by = NULL)}:}{
#'     Aggregate envelope over a geometry column via \code{ST_Envelope_Agg}.
#'   }
#' }
#'
#' @section Table Filter, Join, and Sparse Intersects:
#' \describe{
#'   \item{\code{st_filter(x, y, .predicate = st_intersects)}:}{
#'     Return rows of \code{x} that spatially match \code{y}.
#'   }
#'   \item{\code{st_join(x, y, join = st_intersects)}:}{
#'     Spatial inner join of two lazy tables on \code{geometry}.
#'   }
#'   \item{\code{st_intersects_table(x, y, sparse = TRUE)}:}{
#'     Return a sparse index table of intersecting row pairs.
#'   }
#' }
#'
#' @section SQL Geometry Helpers:
#' Low-level helpers that return \code{dplyr::sql} expressions for use in
#' lazy queries.
#' \describe{
#'   \item{\code{st_make_envelope_sql(xmin, ymin, xmax, ymax)}:}{
#'     Bounding-box envelope expression via \code{ST_MakeEnvelope}.
#'   }
#'   \item{\code{st_point_sql(x_col, y_col)}:}{
#'     Point expression from column names via \code{ST_Point}.
#'   }
#'   \item{\code{st_geomfromtext_sql(wkt)}:}{
#'     Geometry expression from WKT via \code{ST_GeomFromText}.
#'   }
#' }
#'
#' @author Patrick Aboyoun
#'
#' @seealso
#' \itemize{
#'   \item \code{\link[DuckDBDataFrame]{DuckDBTable-class}} for the main class
#'   \item \code{\link[S4Vectors]{RectangularData}} for the base class
#' }
#'
#' @aliases st_as_sfc.DuckDBTable
#'
#' @aliases st_as_binary.DuckDBTable
#' @aliases st_as_text.DuckDBTable
#'
#' @aliases st_dimension
#' @aliases st_dimension.default
#' @aliases st_dimension.DuckDBTable
#' @aliases st_end_point
#' @aliases st_end_point.default
#' @aliases st_end_point.DuckDBTable
#' @aliases st_geometry_type
#' @aliases st_geometry_type.default
#' @aliases st_geometry_type.DuckDBTable
#' @aliases st_is_closed
#' @aliases st_is_closed.default
#' @aliases st_is_closed.DuckDBTable
#' @aliases st_is_empty
#' @aliases st_is_empty.default
#' @aliases st_is_empty.DuckDBTable
#' @aliases st_is_ring
#' @aliases st_is_ring.default
#' @aliases st_is_ring.DuckDBTable
#' @aliases st_is_simple
#' @aliases st_is_simple.default
#' @aliases st_is_simple.DuckDBTable
#' @aliases st_is_valid.DuckDBTable
#' @aliases st_num_geometries
#' @aliases st_num_geometries.default
#' @aliases st_num_geometries.DuckDBTable
#' @aliases st_num_interior_rings
#' @aliases st_num_interior_rings.default
#' @aliases st_num_interior_rings.DuckDBTable
#' @aliases st_num_points
#' @aliases st_num_points.default
#' @aliases st_num_points.DuckDBTable
#' @aliases st_start_point
#' @aliases st_start_point.default
#' @aliases st_start_point.DuckDBTable
#'
#' @aliases st_area.DuckDBTable
#' @aliases st_distance
#' @aliases st_distance.default
#' @aliases st_distance.DuckDBTable
#' @aliases st_length
#' @aliases st_length.default
#' @aliases st_length.DuckDBTable
#' @aliases st_perimeter
#' @aliases st_perimeter.default
#' @aliases st_perimeter.DuckDBTable
#'
#' @aliases st_boundary.DuckDBTable
#' @aliases st_buffer.DuckDBTable
#' @aliases st_build_area
#' @aliases st_build_area.default
#' @aliases st_build_area.DuckDBTable
#' @aliases st_centroid.DuckDBTable
#' @aliases st_collection_extract.DuckDBTable
#' @aliases st_concave_hull.DuckDBTable
#' @aliases st_convex_hull.DuckDBTable
#' @aliases st_envelope
#' @aliases st_envelope.DuckDBTable
#' @aliases st_exterior_ring.DuckDBTable
#' @aliases st_flip_coordinates
#' @aliases st_flip_coordinates.default
#' @aliases st_flip_coordinates.DuckDBTable
#' @aliases st_inscribed_circle
#' @aliases st_inscribed_circle.default
#' @aliases st_inscribed_circle.DuckDBTable
#' @aliases st_line_interpolate
#' @aliases st_line_interpolate.default
#' @aliases st_line_interpolate.DuckDBTable
#' @aliases st_line_merge.DuckDBTable
#' @aliases st_line_project
#' @aliases st_line_project.default
#' @aliases st_line_project.DuckDBTable
#' @aliases st_line_substring
#' @aliases st_line_substring.default
#' @aliases st_line_substring.DuckDBTable
#' @aliases st_make_valid.DuckDBTable
#' @aliases st_minimum_rotated_rectangle.DuckDBTable
#' @aliases st_node.DuckDBTable
#' @aliases st_normalize.DuckDBTable
#' @aliases st_point_on_surface.DuckDBTable
#' @aliases st_reduce_precision
#' @aliases st_reduce_precision.default
#' @aliases st_reduce_precision.DuckDBTable
#' @aliases st_remove_repeated_points
#' @aliases st_remove_repeated_points.default
#' @aliases st_remove_repeated_points.DuckDBTable
#' @aliases st_reverse.DuckDBTable
#' @aliases st_simplify.DuckDBTable
#' @aliases st_voronoi.DuckDBTable
#'
#' @aliases st_contains
#' @aliases st_contains.default
#' @aliases st_contains.DuckDBTable
#' @aliases st_contains_properly
#' @aliases st_contains_properly.default
#' @aliases st_contains_properly.DuckDBTable
#' @aliases st_covered_by
#' @aliases st_covered_by.default
#' @aliases st_covered_by.DuckDBTable
#' @aliases st_covers
#' @aliases st_covers.default
#' @aliases st_covers.DuckDBTable
#' @aliases st_crosses
#' @aliases st_crosses.default
#' @aliases st_crosses.DuckDBTable
#' @aliases st_disjoint
#' @aliases st_disjoint.default
#' @aliases st_disjoint.DuckDBTable
#' @aliases st_equals
#' @aliases st_equals.default
#' @aliases st_equals.DuckDBTable
#' @aliases st_intersects.DuckDBTable
#' @aliases st_is_within_distance
#' @aliases st_is_within_distance.default
#' @aliases st_is_within_distance.DuckDBTable
#' @aliases st_overlaps
#' @aliases st_overlaps.default
#' @aliases st_overlaps.DuckDBTable
#' @aliases st_touches
#' @aliases st_touches.default
#' @aliases st_touches.DuckDBTable
#' @aliases st_within
#' @aliases st_within.default
#' @aliases st_within.DuckDBTable
#' @aliases st_within_properly
#' @aliases st_within_properly.default
#' @aliases st_within_properly.DuckDBTable
#'
#' @aliases st_difference.DuckDBTable
#' @aliases st_intersection.DuckDBTable
#' @aliases st_nearest_points.DuckDBTable
#' @aliases st_union.DuckDBTable
#'
#' @aliases st_collect
#' @aliases st_collect.default
#'
#' @aliases st_filter.DuckDBTable
#' @aliases st_join.DuckDBTable
#' @aliases st_intersects_table
#' @aliases st_make_envelope_sql
#' @aliases st_point_sql
#' @aliases st_geomfromtext_sql
#' @aliases st_union_agg
#' @aliases st_collect_agg
#' @aliases st_envelope_agg
#'
#' @keywords utilities methods
#'
#' @return
#' Method return types are documented in the sections above.
#'
#' @examples
#' spatial_path <- system.file("extdata", "spatial", package = "DuckDBSpatial")
#' df <- DuckDBDataFrame(spatial_path)
#' df <- df[which(!is.na(df$type)), ]
#' query_pt <- sf::st_sfc(sf::st_point(c(30, 10)))
#' filtered <- st_filter(df, query_pt)
#' nrow(filtered)
#' st_make_envelope_sql(0, 0, 100, 100)
#'
#' @name DuckDBTable-spatial
NULL

#' @import methods BiocGenerics
replaceSlots <- BiocGenerics:::replaceSlots

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Internal geometry-to-SQL helpers
###

#' @importFrom sf st_as_text
#' @importFrom dplyr sql
.geom_to_sql <- function(y) {
    if (inherits(y, "sql"))
        return(y)
    if (is.call(y))
        return(y)
    if (is.character(y))
        return(sql(sprintf("ST_GeomFromText('%s')", y)))
    if (inherits(y, "sfg"))
        return(sql(sprintf("ST_GeomFromText('%s')", sf::st_as_text(y))))
    if (inherits(y, "sfc")) {
        if (length(y) != 1L)
            stop("'y' must be a single geometry (sfc of length 1)")
        return(sql(sprintf("ST_GeomFromText('%s')", sf::st_as_text(y[[1L]]))))
    }
    stop("unsupported geometry type: ", class(y)[1L])
}

#' @rdname DuckDBTable-spatial
#' @param xmin,ymin,xmax,ymax Bounding box limits.
#' @export
st_make_envelope_sql <- function(xmin, ymin, xmax, ymax) {
    .st_make_envelope_sql(xmin, ymin, xmax, ymax)
}

.st_point_sql <- function(x_expr, y_expr) {
    call("ST_Point", x_expr, y_expr)
}

#' @importFrom dplyr sql
.st_make_envelope_sql <- function(xmin, ymin, xmax, ymax) {
    sql(sprintf("ST_MakeEnvelope(%s, %s, %s, %s)",
                as.numeric(xmin), as.numeric(ymin),
                as.numeric(xmax), as.numeric(ymax)))
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Creation methods
###

#' @exportS3Method sf::st_as_sfc
#' @importFrom DuckDBDataFrame sql_call
#' @importFrom sf st_as_sfc
st_as_sfc.DuckDBTable <-
function(x, ..., crs = NA_integer_, GeoJSON = FALSE, WKB = FALSE) {
    fun <- if (isTRUE(WKB)) "ST_GeomFromWKB" else if (isTRUE(GeoJSON))
        "ST_GeomFromGeoJSON" else "ST_GeomFromText"
    sql_call(x, fun)
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion methods
###

#' @exportS3Method sf::st_as_binary
#' @importFrom DuckDBDataFrame sql_call
#' @importFrom sf st_as_binary
st_as_binary.DuckDBTable <- function(x, ..., hex = FALSE) {
    fun <- if (isTRUE(hex)) "ST_AsHEXWKB" else "ST_AsWKB"
    sql_call(x, fun)
}

#' @exportS3Method sf::st_as_text
#' @importFrom DuckDBDataFrame sql_call
#' @importFrom sf st_as_text
st_as_text.DuckDBTable <- function(x, ..., geojson = FALSE) {
    fun <- if (isTRUE(geojson)) "ST_AsGeoJSON" else "ST_AsText"
    sql_call(x, fun)
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessor methods
###

#' @export
st_dimension <- function(x, ...) UseMethod("st_dimension")

#' @export
st_dimension.default <- function(x, ...) sf::st_dimension(x, ...)

#' @export
#' @importFrom DuckDBDataFrame sql_call
st_dimension.DuckDBTable <- function(x, ...) {
    sql_call(x, "ST_Dimension")
}

#' @export
st_end_point <- function(x, ...) UseMethod("st_end_point")

#' @export
st_end_point.default <- function(x, ...) {
    stop("st_end_point is not implemented for this class")
}

#' @export
#' @importFrom DuckDBDataFrame sql_call
st_end_point.DuckDBTable <- function(x, ...) {
    sql_call(x, "ST_EndPoint")
}

#' @export
st_geometry_type <- function(x, ...) UseMethod("st_geometry_type")

#' @export
st_geometry_type.default <- function(x, ...) sf::st_geometry_type(x, ...)

#' @export
#' @importFrom DuckDBDataFrame sql_call
st_geometry_type.DuckDBTable <- function(x, ...) {
    sql_call(x, "ST_GeometryType")
}

#' @export
st_is_closed <- function(x, ...) UseMethod("st_is_closed")

#' @export
st_is_closed.default <- function(x, ...) {
    stop("st_is_closed is not implemented for this class")
}

#' @export
#' @importFrom DuckDBDataFrame sql_call
st_is_closed.DuckDBTable <- function(x, ...) {
    sql_call(x, "ST_IsClosed")
}

#' @export
st_is_empty <- function(x, ...) UseMethod("st_is_empty")

#' @export
st_is_empty.default <- function(x, ...) sf::st_is_empty(x, ...)

#' @export
#' @importFrom DuckDBDataFrame sql_call
st_is_empty.DuckDBTable <- function(x, ...) {
    sql_call(x, "ST_IsEmpty")
}

#' @export
st_is_ring <- function(x, ...) UseMethod("st_is_ring")

#' @export
st_is_ring.default <- function(x, ...) {
    stop("st_is_ring is not implemented for this class")
}

#' @export
#' @importFrom DuckDBDataFrame sql_call
st_is_ring.DuckDBTable <- function(x, ...) {
    sql_call(x, "ST_IsRing")
}

#' @export
st_is_simple <- function(x, ...) UseMethod("st_is_simple")

#' @export
st_is_simple.default <- function(x, ...) sf::st_is_simple(x, ...)

#' @export
#' @importFrom DuckDBDataFrame sql_call
st_is_simple.DuckDBTable <- function(x, ...) {
    sql_call(x, "ST_IsSimple")
}

#' @exportS3Method sf::st_is_valid
#' @importFrom DuckDBDataFrame sql_call
#' @importFrom sf st_is_valid
st_is_valid.DuckDBTable <- function(x, ...) {
    sql_call(x, "ST_IsValid")
}

#' @export
st_num_geometries <- function(x, ...) UseMethod("st_num_geometries")

#' @export
st_num_geometries.default <- function(x, ...) {
    stop("st_num_geometries is not implemented for this class")
}
#' @export
#' @importFrom DuckDBDataFrame sql_call
st_num_geometries.DuckDBTable <- function(x, ...) {
    sql_call(x, "ST_NumGeometries")
}

#' @export
st_num_interior_rings <- function(x, ...) UseMethod("st_num_interior_rings")

#' @export
st_num_interior_rings.default <- function(x, ...) {
    stop("st_num_interior_rings is not implemented for this class")
}
#' @export
#' @importFrom DuckDBDataFrame sql_call
st_num_interior_rings.DuckDBTable <- function(x, ...) {
    sql_call(x, "ST_NumInteriorRings")
}

#' @export
st_num_points <- function(x, ...) UseMethod("st_num_points")

#' @export
st_num_points.default <- function(x, ...) {
    stop("st_num_points is not implemented for this class")
}

#' @export
#' @importFrom DuckDBDataFrame sql_call
st_num_points.DuckDBTable <- function(x, ...) {
    sql_call(x, "ST_NumPoints")
}

#' @export
st_start_point <- function(x, ...) UseMethod("st_start_point")

#' @export
st_start_point.default <- function(x, ...) {
    stop("st_start_point is not implemented for this class")
}

#' @export
#' @importFrom DuckDBDataFrame sql_call
st_start_point.DuckDBTable <- function(x, ...) {
    sql_call(x, "ST_StartPoint")
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Measurement methods
###

#' @exportS3Method sf::st_area
#' @importFrom DuckDBDataFrame sql_call
#' @importFrom sf st_area
st_area.DuckDBTable <- function(x, ...) {
    sql_call(x, "ST_Area")
}

#' @export
st_distance <- function(x, y, ...) UseMethod("st_distance")

#' @export
st_distance.default <- function(x, y, ...) sf::st_distance(x, y, ...)

#' @export
#' @importFrom DuckDBDataFrame sql_call
st_distance.DuckDBTable <- function(x, y, ...) {
    sql_call(x, "ST_Distance", .geom_to_sql(y))
}

#' @export
st_length <- function(x, ...) UseMethod("st_length")

#' @export
st_length.default <- function(x, ...) sf::st_length(x, ...)

#' @export
#' @importFrom DuckDBDataFrame sql_call
st_length.DuckDBTable <- function(x, ...) {
    sql_call(x, "ST_Length")
}

#' @export
st_perimeter <- function(x, ...) UseMethod("st_perimeter")

#' @export
st_perimeter.default <- function(x, ...) sf::st_perimeter(x, ...)

#' @export
#' @importFrom DuckDBDataFrame sql_call
st_perimeter.DuckDBTable <- function(x, ...) {
    sql_call(x, "ST_Perimeter")
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Unary operations
###

#' @exportS3Method sf::st_boundary
#' @importFrom DuckDBDataFrame sql_call
#' @importFrom sf st_boundary
st_boundary.DuckDBTable <- function(x) {
    sql_call(x, "ST_Boundary")
}

#' @exportS3Method sf::st_buffer
#' @importFrom DuckDBDataFrame sql_call
#' @importFrom sf st_buffer
st_buffer.DuckDBTable <- function(x, dist, ...) {
    sql_call(x, "ST_Buffer", as.numeric(dist))
}

#' @export
st_build_area <- function(x, ...) UseMethod("st_build_area")

#' @export
st_build_area.default <- function(x, ...) {
    stop("st_build_area is not implemented for this class")
}

#' @export
#' @importFrom DuckDBDataFrame sql_call
st_build_area.DuckDBTable <- function(x, ...) {
    sql_call(x, "ST_BuildArea")
}

#' @exportS3Method sf::st_centroid
#' @importFrom DuckDBDataFrame sql_call
#' @importFrom sf st_centroid
st_centroid.DuckDBTable <- function(x, ...) {
    sql_call(x, "ST_Centroid")
}

.collection_extract_type_to_int <- function(type) {
    type <- match.arg(type, c("POINT", "LINESTRING", "POLYGON"))
    switch(type, POINT = 1L, LINESTRING = 2L, POLYGON = 3L)
}

#' @exportS3Method sf::st_collection_extract
#' @importFrom DuckDBDataFrame sql_call
#' @importFrom sf st_collection_extract
st_collection_extract.DuckDBTable <-
function(x, type = c("POLYGON", "POINT", "LINESTRING"), warn = FALSE, ...) {
    type_int <- .collection_extract_type_to_int(type)
    sql_call(x, "ST_CollectionExtract", type_int)
}

#' @exportS3Method sf::st_concave_hull
#' @importFrom DuckDBDataFrame sql_call
#' @importFrom sf st_concave_hull
st_concave_hull.DuckDBTable <-
function(x, ratio, ..., allow_holes = FALSE) {
    sql_call(x, "ST_ConcaveHull", as.numeric(ratio), allow_holes)
}

#' @exportS3Method sf::st_convex_hull
#' @importFrom DuckDBDataFrame sql_call
#' @importFrom sf st_convex_hull
st_convex_hull.DuckDBTable <- function(x) {
    sql_call(x, "ST_ConvexHull")
}

#' @export
st_envelope <- function(x, ...) UseMethod("st_envelope")

#' @export
#' @importFrom DuckDBDataFrame sql_call
st_envelope.DuckDBTable <- function(x, ...) {
    sql_call(x, "ST_Envelope")
}

#' @exportS3Method sf::st_exterior_ring
#' @importFrom DuckDBDataFrame sql_call
#' @importFrom sf st_exterior_ring
st_exterior_ring.DuckDBTable <- function(x, ...) {
    sql_call(x, "ST_ExteriorRing")
}

#' Apply a 2-D affine coordinate transform to geometries
#'
#' Applies a 2-D affine map to a geometry column via DuckDB \code{ST_Affine}:
#' \eqn{x' = a x + b y + xoff}, \eqn{y' = d x + e y + yoff}. The affine may be a
#' \code{\link{coordinate-transforms}} object (e.g. \code{ctScale},
#' \code{ctRotation}, \code{ctSequence}), lowered to its 2-D coefficients, or a
#' numeric matrix (\code{2x2} linear, \code{2x3} linear+offset, or \code{3x3}
#' homogeneous). Errors if the transform is not 2-D-expressible (e.g. a
#' dimensionality change). For point layers stored as \code{x}/\code{y} columns
#' rather than a geometry column, use \code{\link{transformLayer}}.
#'
#' @param x A \code{DuckDBTable} / \code{DuckDBColumn} geometry, or an \code{sf}
#'   object (default method).
#' @param affine A \code{CoordinateTransform} or affine matrix.
#' @param ... Ignored.
#'
#' @return An object of the same class as \code{x} with transformed geometries.
#'
#' @seealso \code{\link{coordinate-transforms}}, \code{\link{transformLayer}}
#' @export
st_affine <- function(x, affine, ...) UseMethod("st_affine")

#' @export
st_affine.default <- function(x, affine, ...) {
    stop("st_affine is not implemented for this class")
}

#' @export
#' @importFrom DuckDBDataFrame sql_call
st_affine.DuckDBTable <- function(x, affine, ...) {
    co <- .ctTo2DAffine(affine)
    sql_call(x, "ST_Affine", co$a, co$b, co$d, co$e, co$xoff, co$yoff)
}

#' @export
st_flip_coordinates <- function(x, ...) UseMethod("st_flip_coordinates")

#' @export
st_flip_coordinates.default <- function(x, ...) {
    stop("st_flip_coordinates is not implemented for this class")
}

#' @export
#' @importFrom DuckDBDataFrame sql_call
st_flip_coordinates.DuckDBTable <- function(x, ...) {
    sql_call(x, "ST_FlipCoordinates")
}

#' @export
#' @importFrom S4Vectors endoapply
st_inscribed_circle <- function(x, ...) UseMethod("st_inscribed_circle")

#' @export
#' @importFrom sf st_inscribed_circle
st_inscribed_circle.default <- function(x, ...) sf::st_inscribed_circle(x, ...)

#' @export
#' @importFrom S4Vectors endoapply
st_inscribed_circle.DuckDBTable <-
function(x, dTolerance, ..., nQuadSegs = 30) {
    stopifnot(!missing(dTolerance), is.numeric(dTolerance), length(dTolerance) == 1L)
    dtol <- as.numeric(dTolerance)
    FUN <- function(j) {
        mic <- call("ST_MaximumInscribedCircle", j, dtol)
        center <- call("struct_extract", mic, "center")
        radius <- call("struct_extract", mic, "radius")
        call("ST_Buffer", center, radius)
    }
    datacols <- endoapply(x@datacols, FUN)
    replaceSlots(x, datacols = datacols, check = FALSE)
}

#' @export
st_line_interpolate <- function(line, dist, ...) UseMethod("st_line_interpolate")

#' @export
#' @importFrom sf st_line_interpolate
st_line_interpolate.default <- function(line, dist, ...) {
    sf::st_line_interpolate(line, dist, ...)
}

#' @export
#' @importFrom S4Vectors endoapply
st_line_interpolate.DuckDBTable <-
function(line, dist, ..., normalized = FALSE) {
    stopifnot(is.numeric(dist), length(dist) == 1L)
    d <- as.numeric(dist)
    FUN <- function(j) {
        frac <- if (normalized) d
                else call("/", d, call("ST_Length", j))
        call("ST_LineInterpolatePoint", j, frac)
    }
    datacols <- endoapply(line@datacols, FUN)
    replaceSlots(line, datacols = datacols, check = FALSE)
}

#' @exportS3Method sf::st_line_merge
#' @importFrom DuckDBDataFrame sql_call
#' @importFrom sf st_line_merge
st_line_merge.DuckDBTable <- function(x, ..., directed = FALSE) {
    sql_call(x, "ST_LineMerge", directed)
}

#' @export
st_line_project <- function(line, point, ...) UseMethod("st_line_project")

#' @export
#' @importFrom sf st_line_project
st_line_project.default <- function(line, point, ...) {
    sf::st_line_project(line, point, ...)
}

#' @export
#' @importFrom S4Vectors endoapply
st_line_project.DuckDBTable <-
function(line, point, ..., normalized = FALSE) {
    pt_sql <- .geom_to_sql(point)
    FUN <- function(j) {
        frac <- call("ST_LineLocatePoint", j, pt_sql)
        if (normalized) frac else call("*", frac, call("ST_Length", j))
    }
    datacols <- endoapply(line@datacols, FUN)
    replaceSlots(line, datacols = datacols, check = FALSE)
}

#' @export
st_line_substring <- function(line, start, end, ...) UseMethod("st_line_substring")

#' @export
st_line_substring.default <- function(line, start, end, ...) {
    stop("st_line_substring is not implemented for this class")
}

#' @export
#' @importFrom DuckDBDataFrame sql_call
st_line_substring.DuckDBTable <- function(line, start, end, ...) {
    sql_call(line, "ST_LineSubstring", as.numeric(start), as.numeric(end))
}

#' @exportS3Method sf::st_make_valid
#' @importFrom DuckDBDataFrame sql_call
#' @importFrom sf st_make_valid
st_make_valid.DuckDBTable <- function(x, ...) {
    sql_call(x, "ST_MakeValid")
}

#' @exportS3Method sf::st_minimum_rotated_rectangle
#' @importFrom DuckDBDataFrame sql_call
#' @importFrom sf st_minimum_rotated_rectangle
st_minimum_rotated_rectangle.DuckDBTable <- function(x, ...) {
    sql_call(x, "ST_MinimumRotatedRectangle")
}

#' @exportS3Method sf::st_node
#' @importFrom DuckDBDataFrame sql_call
#' @importFrom sf st_node
st_node.DuckDBTable <- function(x, ...) {
    sql_call(x, "ST_Node")
}

#' @exportS3Method sf::st_normalize
#' @importFrom DuckDBDataFrame sql_call
#' @importFrom sf st_normalize
st_normalize.DuckDBTable <- function(x, ...) {
    sql_call(x, "ST_Normalize")
}

#' @exportS3Method sf::st_point_on_surface
#' @importFrom DuckDBDataFrame sql_call
#' @importFrom sf st_point_on_surface
st_point_on_surface.DuckDBTable <- function(x) {
    sql_call(x, "ST_PointOnSurface")
}

#' @export
st_reduce_precision <- function(x, precision, ...) UseMethod("st_reduce_precision")

#' @export
st_reduce_precision.default <- function(x, precision, ...) {
    stop("st_reduce_precision is not implemented for this class")
}

#' @export
#' @importFrom DuckDBDataFrame sql_call
st_reduce_precision.DuckDBTable <- function(x, precision, ...) {
    sql_call(x, "ST_ReducePrecision", as.numeric(precision))
}

#' @export
st_remove_repeated_points <- function(x, ...) UseMethod("st_remove_repeated_points")

#' @export
st_remove_repeated_points.default <- function(x, ...) {
    stop("st_remove_repeated_points is not implemented for this class")
}

#' @export
#' @importFrom DuckDBDataFrame sql_call
st_remove_repeated_points.DuckDBTable <- function(x, ...) {
    sql_call(x, "ST_RemoveRepeatedPoints")
}

#' @exportS3Method sf::st_reverse
#' @importFrom DuckDBDataFrame sql_call
#' @importFrom sf st_reverse
st_reverse.DuckDBTable <- function(x) {
    sql_call(x, "ST_Reverse")
}

#' @exportS3Method sf::st_simplify
#' @importFrom DuckDBDataFrame sql_call
#' @importFrom sf st_simplify
st_simplify.DuckDBTable <-
function(x, preserveTopology = FALSE, dTolerance = 0.0, ...) {
    fun <- if (isTRUE(preserveTopology)) "ST_SimplifyPreserveTopology"
           else "ST_Simplify"
    sql_call(x, fun, as.numeric(dTolerance))
}

#' @exportS3Method sf::st_voronoi
#' @importFrom DuckDBDataFrame sql_call
#' @importFrom sf st_voronoi
st_voronoi.DuckDBTable <- function(x, ...) {
    sql_call(x, "ST_VoronoiDiagram")
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Binary spatial predicates
###

#' @export
st_contains <- function(x, y, ...) UseMethod("st_contains")

#' @export
st_contains.default <- function(x, y, ...) sf::st_contains(x, y, ...)

#' @export
#' @importFrom DuckDBDataFrame sql_call
st_contains.DuckDBTable <- function(x, y, ...) {
    sql_call(x, "ST_Contains", .geom_to_sql(y))
}

#' @export
st_contains_properly <- function(x, y, ...) UseMethod("st_contains_properly")

#' @export
st_contains_properly.default <- function(x, y, ...) {
    sf::st_contains_properly(x, y, ...)
}

#' @export
#' @importFrom DuckDBDataFrame sql_call
st_contains_properly.DuckDBTable <- function(x, y, ...) {
    sql_call(x, "ST_ContainsProperly", .geom_to_sql(y))
}

#' @export
st_covered_by <- function(x, y, ...) UseMethod("st_covered_by")

#' @export
st_covered_by.default <- function(x, y, ...) sf::st_covered_by(x, y, ...)

#' @export
#' @importFrom DuckDBDataFrame sql_call
st_covered_by.DuckDBTable <- function(x, y, ...) {
    sql_call(x, "ST_CoveredBy", .geom_to_sql(y))
}

#' @export
st_covers <- function(x, y, ...) UseMethod("st_covers")

#' @export
st_covers.default <- function(x, y, ...) sf::st_covers(x, y, ...)

#' @export
#' @importFrom DuckDBDataFrame sql_call
st_covers.DuckDBTable <- function(x, y, ...) {
    sql_call(x, "ST_Covers", .geom_to_sql(y))
}

#' @export
st_crosses <- function(x, y, ...) UseMethod("st_crosses")

#' @export
st_crosses.default <- function(x, y, ...) sf::st_crosses(x, y, ...)

#' @export
#' @importFrom DuckDBDataFrame sql_call
st_crosses.DuckDBTable <- function(x, y, ...) {
    sql_call(x, "ST_Crosses", .geom_to_sql(y))
}

#' @export
st_disjoint <- function(x, y, ...) UseMethod("st_disjoint")

#' @export
st_disjoint.default <- function(x, y, ...) sf::st_disjoint(x, y, ...)

#' @export
#' @importFrom DuckDBDataFrame sql_call
st_disjoint.DuckDBTable <- function(x, y, ...) {
    sql_call(x, "ST_Disjoint", .geom_to_sql(y))
}

#' @export
st_equals <- function(x, y, ...) UseMethod("st_equals")

#' @export
st_equals.default <- function(x, y, ...) sf::st_equals(x, y, ...)

#' @export
#' @importFrom DuckDBDataFrame sql_call
st_equals.DuckDBTable <- function(x, y, ...) {
    sql_call(x, "ST_Equals", .geom_to_sql(y))
}

#' @exportS3Method sf::st_intersects
#' @importFrom DuckDBDataFrame sql_call
#' @importFrom sf st_intersects
st_intersects.DuckDBTable <- function(x, y, ...) {
    sql_call(x, "ST_Intersects", .geom_to_sql(y))
}

#' @export
st_is_within_distance <- function(x, y, ...) UseMethod("st_is_within_distance")

#' @export
st_is_within_distance.default <- function(x, y, ...) {
    sf::st_is_within_distance(x, y, ...)
}

#' @export
#' @importFrom DuckDBDataFrame sql_call
st_is_within_distance.DuckDBTable <- function(x, y, dist, ...) {
    sql_call(x, "ST_DWithin", .geom_to_sql(y), as.numeric(dist))
}

#' @export
st_overlaps <- function(x, y, ...) UseMethod("st_overlaps")

#' @export
st_overlaps.default <- function(x, y, ...) sf::st_overlaps(x, y, ...)

#' @export
#' @importFrom DuckDBDataFrame sql_call
st_overlaps.DuckDBTable <- function(x, y, ...) {
    sql_call(x, "ST_Overlaps", .geom_to_sql(y))
}

#' @export
st_touches <- function(x, y, ...) UseMethod("st_touches")

#' @export
st_touches.default <- function(x, y, ...) sf::st_touches(x, y, ...)

#' @export
#' @importFrom DuckDBDataFrame sql_call
st_touches.DuckDBTable <- function(x, y, ...) {
    sql_call(x, "ST_Touches", .geom_to_sql(y))
}

#' @export
st_within <- function(x, y, ...) UseMethod("st_within")

#' @export
st_within.default <- function(x, y, ...) sf::st_within(x, y, ...)

#' @export
#' @importFrom DuckDBDataFrame sql_call
st_within.DuckDBTable <- function(x, y, ...) {
    sql_call(x, "ST_Within", .geom_to_sql(y))
}

#' @export
st_within_properly <- function(x, y, ...) UseMethod("st_within_properly")

#' @export
st_within_properly.default <- function(x, y, ...) {
    t(sf::st_contains_properly(y, x, ...))
}

#' @export
#' @importFrom DuckDBDataFrame sql_call
st_within_properly.DuckDBTable <- function(x, y, ...) {
    sql_call(x, "ST_WithinProperly", .geom_to_sql(y))
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Binary spatial set operations
###

#' @exportS3Method sf::st_difference
#' @importFrom DuckDBDataFrame sql_call
#' @importFrom sf st_difference
st_difference.DuckDBTable <- function(x, y, ...) {
    sql_call(x, "ST_Difference", .geom_to_sql(y))
}

#' @exportS3Method sf::st_intersection
#' @importFrom DuckDBDataFrame sql_call
#' @importFrom sf st_intersection
st_intersection.DuckDBTable <- function(x, y, ...) {
    sql_call(x, "ST_Intersection", .geom_to_sql(y))
}

#' @exportS3Method sf::st_nearest_points
#' @importFrom DuckDBDataFrame sql_call
#' @importFrom sf st_nearest_points
st_nearest_points.DuckDBTable <- function(x, y, ...) {
    sql_call(x, "ST_ShortestLine", .geom_to_sql(y))
}

#' @exportS3Method sf::st_union
#' @importFrom DuckDBDataFrame sql_call
#' @importFrom sf st_union
st_union.DuckDBTable <- function(x, y, ...) {
    if (missing(y))
        stop("'y' is required for st_union on DuckDBTable; ",
             "use st_union on DuckDBColumn for aggregate union")
    sql_call(x, "ST_Union", .geom_to_sql(y))
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Aggregate spatial operations (generic and default only)
###

#' @export
st_collect <- function(x, ...) UseMethod("st_collect")

#' @export
st_collect.default <- function(x, ...) sf::st_combine(x, ...)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Spatial filter, join, sparse intersects
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#' @rdname DuckDBTable-spatial
#' @param wkt A WKT character string.
#' @export
st_geomfromtext_sql <- function(wkt) .geom_to_sql(wkt)

#' @rdname DuckDBTable-spatial
#' @param x_col,y_col Column names for x and y coordinates.
#' @importFrom dplyr sql
#' @export
st_point_sql <- function(x_col, y_col) {
    sql(sprintf('ST_Point("%s", "%s")', x_col, y_col))
}

#' @importFrom DuckDBDataFrame DuckDBDataFrame tblconn dbconn
#' @importFrom dbplyr sql_render
#' @importFrom dplyr sql tbl
.lazy_sql_ddf <- function(conn, sql_txt) {
    DuckDBDataFrame(tbl(conn, sql(sql_txt)))
}

#' @rdname DuckDBTable-spatial
#' @param x A \code{DuckDBTable} or \code{DuckDBDataFrame}.
#' @param y A geometry to test against.
#' @param sparse Logical; return a lazy table when \code{TRUE}.
#' @param ... Ignored.
#' @importFrom DuckDBDataFrame DuckDBDataFrame tblconn dbconn
#' @importFrom dbplyr sql_render
#' @importFrom DBI dbGetQuery
#' @export
st_intersects_table <- function(x, y, sparse = TRUE, ...) {
    if (!is(x, "DuckDBTable"))
        stop("'x' must be a DuckDBTable or DuckDBDataFrame")
    y_sql <- .geom_to_sql(y)
    conn <- dbconn(x)
    x_q <- sql_render(tblconn(x, select = FALSE))
    sql_txt <- sprintf(
        paste0("SELECT row_number() OVER () AS id_x, t.__rowid__ AS id_y ",
               "FROM (SELECT *, row_number() OVER () AS __rowid__ FROM (%s)) t ",
               "WHERE ST_Intersects(t.geometry, %s)"),
        x_q, as.character(y_sql))
    if (isTRUE(sparse)) {
        .lazy_sql_ddf(conn, sql_txt)
    } else {
        sql_txt
    }
}

#' @rdname DuckDBTable-spatial
#' @param x A \code{DuckDBTable} or \code{DuckDBDataFrame}.
#' @param y A geometry to test against.
#' @param .predicate Spatial predicate function (default \code{st_intersects}).
#' @param ... Passed to \code{.predicate}.
#' @exportS3Method sf::st_filter
#' @importFrom sf st_filter st_intersects
st_filter.DuckDBTable <- function(x, y, ..., .predicate = st_intersects) {
    geom <- x[["geometry"]]
    hits <- .predicate(geom, y, ...)
    if (is(hits, "DuckDBColumn")) {
        x[hits, , drop = FALSE]
    } else {
        hits <- as.logical(hits)
        idx <- which(hits)
        if (length(idx))
            x[idx, , drop = FALSE]
        else
            x[integer(0L), , drop = FALSE]
    }
}

# Map an sf spatial-predicate function to its DuckDB ST_* function name, so
# st_join() honors the `join` argument instead of always using ST_Intersects.
.st_predicate_sql_name <- function(join) {
    preds <- c(
        st_intersects = "ST_Intersects", st_within = "ST_Within",
        st_contains = "ST_Contains", st_covers = "ST_Covers",
        st_covered_by = "ST_CoveredBy", st_overlaps = "ST_Overlaps",
        st_touches = "ST_Touches", st_equals = "ST_Equals",
        st_crosses = "ST_Crosses", st_disjoint = "ST_Disjoint")
    for (nm in names(preds)) {
        fn <- tryCatch(getExportedValue("sf", nm), error = function(e) NULL)
        if (!is.null(fn) && identical(join, fn)) {
            return(preds[[nm]])
        }
    }
    stop("st_join() on a DuckDBTable supports an sf spatial predicate ",
         "(st_intersects, st_within, st_contains, st_covers, st_covered_by, ",
         "st_overlaps, st_touches, st_equals, st_crosses, st_disjoint); ",
         "the supplied 'join' was not recognized")
}

#' @rdname DuckDBTable-spatial
#' @param x A \code{DuckDBTable} or \code{DuckDBDataFrame}.
#' @param y A \code{DuckDBTable} or \code{DuckDBDataFrame} to join against.
#' @param join Spatial predicate function (default \code{st_intersects}).
#' @param ... Ignored.
#' @exportS3Method sf::st_join
#' @importFrom DuckDBDataFrame DuckDBDataFrame tblconn dbconn
#' @importFrom dbplyr sql_render
#' @importFrom sf st_join st_intersects
st_join.DuckDBTable <- function(x, y, join = st_intersects, ...) {
    pred <- .st_predicate_sql_name(join)
    conn <- dbconn(x)
    x_q <- sql_render(tblconn(x, select = FALSE))
    y_q <- sql_render(tblconn(y, select = FALSE))
    sql_txt <- sprintf(
        "SELECT x.*, y.* FROM (%s) x INNER JOIN (%s) y ON %s(x.geometry, y.geometry)",
        x_q, y_q, pred)
    .lazy_sql_ddf(conn, sql_txt)
}

#' @importFrom DuckDBDataFrame DuckDBTable tblconn
#' @importFrom dplyr group_by summarize
#' @importFrom S4Vectors new2
.spatial_geometry_agg <- function(x, fun, by = NULL) {
    if (is(x, "DuckDBTable"))
        x <- x@datacols[[1L]]
    if (!is(x, "DuckDBColumn"))
        stop("'x' must be a DuckDBColumn or DuckDBTable with a geometry column")
    table <- x@table
    geom <- table@datacols[[1L]]
    conn <- tblconn(x, select = FALSE)
    aggr <- list(geometry = call("ST_AsText", call(fun, geom)))
    if (is.null(by)) {
        conn <- summarize(conn, !!!aggr)
    } else {
        conn <- summarize(group_by(conn, !!!lapply(by, as.name)), !!!aggr)
    }
    new2("DuckDBColumn", table = DuckDBTable(conn, datacols = "geometry"), check = FALSE)
}

#' @rdname DuckDBTable-spatial
#' @param x A \code{DuckDBColumn} or \code{DuckDBTable} geometry column.
#' @param by Optional grouping column name(s).
#' @param ... Ignored.
#' @export
st_union_agg <- function(x, by = NULL, ...) {
    .spatial_geometry_agg(x, "ST_Union_Agg", by = by)
}

#' @rdname DuckDBTable-spatial
#' @param x A \code{DuckDBColumn} or \code{DuckDBTable} geometry column.
#' @param by Optional grouping column name(s).
#' @param ... Ignored.
#' @export
st_collect_agg <- function(x, by = NULL, ...) {
    if (is(x, "DuckDBTable"))
        x <- x@datacols[[1L]]
    if (!is(x, "DuckDBColumn"))
        stop("'x' must be a DuckDBColumn or DuckDBTable with a geometry column")
    table <- x@table
    geom <- table@datacols[[1L]]
    conn <- tblconn(x, select = FALSE)
    aggr <- list(geometry = call("ST_AsText", call("ST_Collect", call("list", geom))))
    if (is.null(by)) {
        conn <- summarize(conn, !!!aggr)
    } else {
        conn <- summarize(group_by(conn, !!!lapply(by, as.name)), !!!aggr)
    }
    new2("DuckDBColumn", table = DuckDBTable(conn, datacols = "geometry"), check = FALSE)
}

#' @rdname DuckDBTable-spatial
#' @param x A \code{DuckDBColumn} or \code{DuckDBTable} geometry column.
#' @param by Optional grouping column name(s).
#' @param ... Ignored.
#' @export
st_envelope_agg <- function(x, by = NULL, ...) {
    .spatial_geometry_agg(x, "ST_Envelope_Agg", by = by)
}
