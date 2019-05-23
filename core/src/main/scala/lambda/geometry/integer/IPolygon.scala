package lambda.geometry.integer

import java.awt.Polygon

import lambda.geometry
import lambda.geometry.floating.{FPolygon, FPolygonUtils}
import lambda.geometry.{GeometryException, getEdges, getTriples}
import lambda.geometry.integer.IPointUtils.direction

import scala.util.Random

/**
  *
  * A polygon with integer-point coordinates
  *
  * @author Ilya Sergey
  */
case class IPolygon(vertices: Seq[IPoint]) {

  import lambda.geometry.Turn._


  override def toString = vertices.mkString(",")

  def edges: Seq[ISegment] =
    getEdges(vertices).map { case (a, b) => ISegment(a, b) }

  /**
    * Since we're working with rectilinear polygons, this will come useful
    */
  def horizontalEdges: Seq[ISegment] = edges.filter { case ISegment(a, b) => a.y == b.y }

  def verticalEdges: Seq[ISegment] = edges.filter { case ISegment(a, b) => a.x == b.x }

  lazy val toFPolygon: FPolygon = FPolygon(vertices.map(_.toFPoint))
  
  lazy val toJPolygon: Polygon = {
    val (xs, ys) = vertices.unzip(_.toPair)
    new Polygon(xs.toArray, ys.toArray, xs.size)
  }

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
    val fs = triplesV.filterNot { case (a, b, c) => IPointUtils.crossProduct(c - a, b - a) == 0 }
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
  def containsCellOld(sq: IPoint): Boolean = {
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

  def containsCell(sq: IPoint): Boolean = containsCellViaAWT(sq)
  
  private def containsCellViaAWT(sq: IPoint) = {
    val (sx, sy) = sq.toPair
    val xProbe: Double = sx.toDouble + 0.5 // Take a centre of the square
    val yProbe: Double = sy.toDouble + 0.5 
    toJPolygon.contains(xProbe, yProbe)
  }


  def getMinXY = {
    val minx = vertices.minBy { case IPoint(x, y) => x }.x
    val miny = vertices.minBy { case IPoint(x, y) => y }.y
    IPoint(minx, miny)
  }

  /**
    * Make so the polygon is located only in the positive x-y coordinates and touches both axes
    */
  def shiftToOrigin = {
    val mp = getMinXY
    IPolygon(vertices.map { v => v - mp })
  }

  def +(p: IPoint) = IPolygon(vertices.map { _ + p })


  /**
    * Returns a bounding box of a room
    */
  def boundingBox: ((Int, Int), (Int, Int)) = {
    val bottomLeft = vertices.head.toPair
    val topRight = vertices.head.toPair

    val ((xL, yL), (xR, yR)) = vertices.foldLeft((bottomLeft, topRight)) {
      case (((xl, yl), (xr, yr)), IPoint(x, y)) =>
        ((math.min(xl, x), math.min(yl, y)),
          (math.max(xr, x), math.max(yr, y)))
    }

    ((xL, yL), (xR, yR))
  }

  def dimensions: (Int, Int) = {
    val ((xL, yL), (xR, yR)) = boundingBox
    (xR - xL, yR - yL)
  }

  /**
    *
    * Convert a polygon to matrix
    *
    * @return
    */
  def toMatrix: (Array[Array[Int]], Int, Int) = {
    val ((xl, yl), (xr, yr)) = boundingBox
    if (xl < 0 || yl < 0)
      throw GeometryException("Polygon with negative lower coordinates", this)
    if (xr <= 0 || yr <= 0)
      throw GeometryException("Polygon with non-positive upper coordinates", this)

    val matrix = Array.fill(xr) {
      Array.fill(yr) {
        1
      }
    }

    for {i <- 0 until xr
         j <- 0 until yr
         if containsCell(IPoint(i, j))} {
      matrix(i)(j) = 0
    }

    (matrix, xr, yr)
  }

  def pixelsUsed = {
    val (matrix, xr, yr) = shiftToOrigin.toMatrix
    var count = 0
    for {
      i <- 0 until xr
      j <- 0 until yr
      if matrix(i)(j) == 0
    } {
      count = count + 1
    }
    (count, count.toDouble / (xr * yr))
  }

  def printInAscii = {
    val (matrix, xsize, ysize) = toMatrix
    for (i <- 0 until xsize + 2) print("X")
    println()
    for (j1 <- 1 to ysize) {
      val j = ysize - j1
      print("X")
      for (i <- 0 until xsize) {
        val b = matrix(i)(j)
        print(if (b > 0) "X" else " ")

      }
      print("X")
      println()
    }
    for (i <- 0 until xsize + 2) print("X")
    println()
  }

  /**
    * Assuming this and other polygons are rectilinear
    *
    * @param other another rectilinear polygon
    */
  def containsPolygonProperly(other: IPolygon): Boolean = {
    if (other.vertices.size < 4) return false

    val intersects = this.intersectPolygon(other)
    if (intersects) return false

    val v = other.vertices.head

    // Should contain a square from another polygon
    this.containsCell(v)
  }


  def randomCellWithin: IPoint = {
    val ((xl, yl), (xr, yr)) = boundingBox
    var x = xl + Random.nextInt(xr - xl)
    var y = yl + Random.nextInt(yr - yl)
    while (!containsCell(IPoint(x, y))) {
      x = xl + Random.nextInt(xr - xl)
      y = yl + Random.nextInt(yr - yl)
    }
    val res = IPoint(x, y)
    assert(this.containsCell(res))
    res
  }
  
  def leftMostBottomCell: IPoint = {
    val IPoint(minx, _) = getMinXY
    val minLeft = vertices.filter(_.x == minx).minBy(_.y)
    minLeft
  }


}
