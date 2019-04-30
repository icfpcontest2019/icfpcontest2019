package lambda.geometry.floating

/**
  * Implementation of points in Cartesian and polar coordinates
  *
  * @author Ilya Sergey
  */

import lambda.geometry._
import lambda.geometry.integer.IPoint

trait MetricPoint[T] {
  def distanceTo(other: T) : Double
}

object FPoint {
  val MinValue = FPoint(Double.MinValue, Double.MinValue)
  val MaxValue = FPoint(Double.MaxValue, Double.MaxValue)
}

/**
  * A point in Cartesian coordinates
  */
case class FPoint(x: Double, y: Double) extends EpsEq[FPoint] with MetricPoint[FPoint] {
  def -(p: FPoint): FPoint = FPoint(x - p.x, y - p.y)

  def +(d: Direction): FPoint = FPoint(x + d.dx, y + d.dy)

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

  def =~=(q: FPoint) = (x =~= q.x) && (y =~= q.y)

  // Find the greatest lower bound of two points (pointwise order)
  def glb (other: FPoint): FPoint = {
    FPoint(
      x min other.x,
      y min other.y
    )
  }

  // Find the lowest upper bound of two points (pointwise order)
  def lub (other: FPoint): FPoint = {
    FPoint(
      x max other.x,
      y max other.y
    )
  }

  override def distanceTo(other: FPoint): Double =
    math.sqrt(FPointUtils.squaredLengthBA(this, other))

  // Returns whether we can reach the other point by a straight line, without
  // intersecting any of the obstacle polygons.
  def reaches(other: FPoint, obstacles: Seq[FPolygon]): Boolean = {
    val straight = FSegment(this, other)

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

  implicit def toCart: FPoint = {
    val x = rad * scala.math.cos(phi)
    val y = rad * scala.math.sin(phi)
    FPoint(x, y)
  }

  def rotateClockWise(z: Double) =
    PointPolar(rad, normalize(this.phi - z, 2 * PI))

  def =~=(q: PointPolar) = (rad =~= q.rad) && (phi =~= q.phi)
}





/**
  * Utility methods for points
  */
object FPointUtils {

  import Turn._

  val origin2D = FPoint(0, 0)

  implicit def _point2D(p: (AnyVal, AnyVal)): FPoint =
    FPoint(p._1.asInstanceOf[ToDouble].toDouble,
      p._2.asInstanceOf[ToDouble].toDouble)

  implicit def _polar2Cart(p: FPoint): PointPolar = p.toPolar
  implicit def _cart2Polar(p: PointPolar): FPoint = p.toCart
  implicit def _point2DtoDir(p: FPoint): Direction = Direction(p.x, p.y)

  /**
    * Cross-product of three points on a plane
    */

  def roundPoint(p: FPoint) = FPoint(roundToClosestInt(p.x), roundToClosestInt(p.y))

  def crossProduct(p1: FPoint, p2: FPoint): Double =
    p1.x * p2.y - p2.x * p1.y

  def dotProduct(p1: FPoint, p2: FPoint): Double =
    p1.x * p2.x + p1.y * p2.y

  def squaredLengthBA(a: FPoint, b: FPoint) =
    (b.x - a.x) * (b.x - a.x) + (b.y - a.y) * (b.y - a.y)


  /**
    * Turn between three points
    * +1 -- left turn
    * -1 -- right turn
    * 0 -- points are collinear
    */
  def direction(x: FPoint, y: FPoint, z: FPoint): Turn = {
    val sig = scala.math.signum(crossProduct(z - x, y - x))
    sig match {
      case 1 => RightTurn
      case -1 => LeftTurn
      case 0 => NoTurn
    }
  }
}
