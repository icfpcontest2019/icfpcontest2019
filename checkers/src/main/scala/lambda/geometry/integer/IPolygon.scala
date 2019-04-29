package lambda.geometry.integer

import lambda.geometry
import lambda.geometry.floating.{FPolygon, FPolygonUtils}
import lambda.geometry.{getEdges, getTriples}
import lambda.geometry.integer.IPointUtils.direction

/**
  *
  * A polygon with integer-point coordinates
  *
  * @author Ilya Sergey
  */
case class IPolygon(vertices: Seq[IPoint]) {

  import lambda.geometry.Turn._

  def edges: Seq[ISegment] =
    getEdges(vertices).map { case (a, b) => ISegment(a, b) }

  /**
    * Since we're working with rectilinear polygons, this will come useful
    */
  def horizontalEdges: Seq[ISegment] = edges.filter { case ISegment(a, b) => a.y == b.y }

  def verticalEdges: Seq[ISegment] = edges.filter { case ISegment(a, b) => a.x == b.x }

  def toFPolygon: FPolygon = FPolygon(vertices.map(_.toFPoint))

  def isRectilinear: Boolean = {
    val es = edges
    if (es.isEmpty) return false
    val esPaired = es.zip(es.tail ++ Seq(es.head))
    val allOrthogonal = esPaired.forall { case (e1, e2) => e1.dotProduct(e2) == 0 }
    allOrthogonal
  }

  def isWellFormed: Boolean = {
    if (vertices.size < 3) return false

    val nonZeroEdges = edges.forall(e => e.squaredLength > 0)
    val noIntersect = FPolygonUtils.noSelfIntersections(toFPolygon)

    val triples = geometry.getTriples(vertices)
    val countLeftTurn = triples.count { case (a, b, c) => direction(a, b, c) == LeftTurn }
    val countRightTurn = triples.count { case (a, b, c) => direction(a, b, c) == RightTurn }
    val countNoTurn = triples.count { case (a, b, c) => direction(a, b, c) == NoTurn }

    nonZeroEdges && // No zero-length edges
      noIntersect && // No self-intersections
      countLeftTurn > countRightTurn && // All polygon is on the left
      countNoTurn == 0 // No 0-degree turns

  }

  /**
    * Remove aligned vertices from a polygon boundary and return a new polygon
    */
  def removeAligned: IPolygon = {
    val triplesV = getTriples(vertices)
    val fs = triplesV.filterNot{ case (a, b, c) => IPointUtils.crossProduct(c - a, b - a) == 0 }
    IPolygon(fs.map(_._1))
  }


  /**
    * Not very efficient (O(n^^2)), but suffices for our purposes
    */
  def intersectPolygon(other: IPolygon) = {
    val es = this.edges
    val esOther = other.edges
    es.exists(e => esOther.exists(e1 => e.intersects(e1)))
  }

  /**
    * Only applicable if this polygon is rectilinear.
    *
    * The s square is given with it bottom-left corner
    *
    */
  def containsSquare(sq: IPoint) = {
    val (sx, sy) = sq.toPair
    val verticalEdgesRight = verticalEdges.filter { case ISegment(a, _) => a.x > sx }

    val yProbe: Double = sy.toDouble + 0.5 // Take a centre of the square

    val intersectedCount = verticalEdgesRight.count { case ISegment(a, b) =>
      val ay = a.y
      val by = b.y
      math.min(ay, by).toDouble <= yProbe && yProbe <= math.max(ay, by).toDouble
    }
    intersectedCount % 2 == 1
  }


  def getMinXY = {
    val minx = vertices.minBy { case IPoint(x, y) => x }.x
    val miny = vertices.minBy {case IPoint(x, y) => y}.y
    IPoint(minx, miny)
  }

  /**
    * Make so the polygon is located only in the positive x-y coordinates and touches both axes
    */
  def shiftToOrigin = {
    val mp = getMinXY
    IPolygon(vertices.map{v => v - mp})
  }


  def boundingBox = {
    val bottomLeft = vertices.head.toPair
    val topRight = vertices.head.toPair

    vertices.foldLeft((bottomLeft, topRight)) {
      case (((xl, yl), (xr, yr)), IPoint(x, y)) =>
        ((math.min(xl, x), math.min(yl, y)),
          (math.max(xr, x), math.max(yr, y)))
    }
  }


  /*
  TODO:

  * Check if left-sided

   */

}
