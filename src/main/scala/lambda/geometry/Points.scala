package lambda.geometry

import CommonUtils._

/**
  * Implementation of points in Cartesian and polar coordinates
  *
  * @author Ilya Sergey
  */

trait MetricPoint[T] {
  def distanceTo(other: T) : Double
}

object Point2D {
  val MinValue = Point2D(Double.MinValue, Double.MinValue)
  val MaxValue = Point2D(Double.MaxValue, Double.MaxValue)
}

/**
  * A point in Cartesian coordinates
  */
case class Point2D(x: Double, y: Double) extends EpsEq[Point2D] with MetricPoint[Point2D] {
  def -(p: Point2D): Point2D = Point2D(x - p.x, y - p.y)

  def +(d: Direction): Point2D = Point2D(x + d.dx, y + d.dy)

  implicit def toPolar: PointPolar = {
    val r = scala.math.hypot(x, y)
    val phi = scala.math.atan2(y, x)
    val phi1 = if (phi < 0) phi + 2 * PI else phi
    val phi2 = if (phi1 == 2 * PI) 0 else phi1
    assert(0.0 <=~ phi2 && phi2 < 2 * PI)
    PointPolar(r, phi2)
  }


  override def toString = {
    val a = if (x =~= x.toInt.toDouble) x.toInt.toString else x.toString
    val b = if (y =~= y.toInt.toDouble) y.toInt.toString else y.toString
    s"($a, $b)"
  }

  def =~=(q: Point2D) = (x =~= q.x) && (y =~= q.y)

  // Find the greatest lower bound of two points (pointwise order)
  def glb (other: Point2D): Point2D = {
    Point2D(
      x min other.x,
      y min other.y
    )
  }

  // Find the lowest upper bound of two points (pointwise order)
  def lub (other: Point2D): Point2D = {
    Point2D(
      x max other.x,
      y max other.y
    )
  }

  override def distanceTo(other: Point2D): Double =
    math.sqrt(PointUtils.squaredLengthBA(this, other))

  // Returns whether we can reach the other point by a straight line, without
  // intersecting any of the obstacle polygons.
  def reaches(other: Point2D, obstacles: Seq[Polygon]): Boolean = {
    val straight = Segment(this, other)

    // We can draw a straight line to the other obstacles if this straight line
    // will not intersect any of the edges of the obstacle polygons.
    !obstacles.exists(obstacle => obstacle intersectProper straight)
  }
}

case class Direction(dx: Double, dy: Double) {

  // 2-dimensional cross-product
  def **(d: Direction) = this.dx * d.dy - this.dy * d.dx

  // Scalar product
  def *(d: Direction) = dx * d.dx + dy * d.dy

  def scaleBy(k: Double) = Direction(k * dx, k * dy)

}


/**
  * A point in polar coordinates
  */
case class PointPolar(rad: Double, phi: Double) extends EpsEq[PointPolar] {

  implicit def toCart: Point2D = {
    val x = rad * scala.math.cos(phi)
    val y = rad * scala.math.sin(phi)
    Point2D(x, y)
  }

  def rotateClockWise(z: Double) =
    PointPolar(rad, normalize(this.phi - z, 2 * PI))

  def =~=(q: PointPolar) = (rad =~= q.rad) && (phi =~= q.phi)
}

/**
  * Turns
  */
object Turn extends Enumeration {
  type Turn = Value
  val RightTurn, LeftTurn, NoTurn = Value

  implicit def _turn2Int(t: Turn): Int = t match {
    case RightTurn => -1
    case LeftTurn => 1
    case NoTurn => 0
  }

  implicit def _turn2Bool(t: Turn): Boolean = t match {
    case RightTurn => true
    case LeftTurn => false
    case NoTurn => throw GeometryException("Cannot convert NoTurn to Boolean", None)
  }

}

import Turn._


/**
  * Utility methods for points
  */
object PointUtils {

  val origin2D = Point2D(0, 0)

  implicit def _point2D(p: (AnyVal, AnyVal)): Point2D =
    Point2D(p._1.asInstanceOf[ToDouble].toDouble,
      p._2.asInstanceOf[ToDouble].toDouble)

  implicit def _polar2Cart(p: Point2D): PointPolar = p.toPolar
  implicit def _cart2Polar(p: PointPolar): Point2D = p.toCart
  implicit def _point2DtoDir(p: Point2D): Direction = Direction(p.x, p.y)

  /**
    * Cross-product of three points on a plane
    */

  def roundPoint(p: Point2D) = Point2D(roundToClosestInt(p.x), roundToClosestInt(p.y))

  def crossProduct(a: Point2D, b: Point2D, c: Point2D): Double =
    (b.x - a.x) * (c.y - a.y) - (b.y - a.y) * (c.x - a.x)

  def dotProduct(a: Point2D, b: Point2D, c: Point2D) =
    (c.x - a.x) * (b.x - a.x) + (c.y - a.y) * (b.y - a.y)

  def squaredLengthBA(a: Point2D, b: Point2D) =
    (b.x - a.x) * (b.x - a.x) + (b.y - a.y) * (b.y - a.y)


  /**
    * Turn between three points
    * +1 -- left turn
    * -1 -- right turn
    * 0 -- points are collinear
    */
  def direction(x: Point2D, y: Point2D, z: Point2D): Turn = {
    val sig = scala.math.signum(crossProduct(x, y, z))
    sig match {
      case 1 => LeftTurn
      case -1 => RightTurn
      case 0 => NoTurn
    }
  }
}
