package lambda.geometry

import PointUtils._

/**
  * @author Ilya Sergey
  */

case class Segment(a: Point2D, b: Point2D) extends EpsEq[Segment] {
  def toPair: (Point2D, Point2D) = (a, b)

  override def =~=(q: Segment) = (a =~= q.a) && (b =~= q.b)

  def direction = Direction(b.x - a.x, b.y - a.y)

  def middle: Point2D = a + Direction((b.x - a.x) / 2, (b.y - a.y) / 2)

  def length = math.sqrt(squaredLengthBA(a, b))
  override def toString = s"[$a, $b]"

  def flip = Segment(b, a)

  def contains(p: Point2D) : Boolean = SegmentUtils.pointOnSegment(p, this)

  def contains(other: Segment) : Boolean = contains(other.a) && contains(other.b)

  /**
    * Get polar angle of the segment
    */
  def angle = Point2D(direction.dx, direction.dy).phi

  def intersect(other: Segment) = SegmentUtils.intersect(this, other)

  def intersectProper(other: Segment) = SegmentUtils.intersectProper(this, other)

  def intersectInterior(other: Segment) = SegmentUtils.intersectInterior(this, other)

  def intersection(other: Segment) = SegmentUtils.intersectSegments(this, other)

  def collinear(other: Segment) = SegmentUtils.segmentsCollinear(this, other)

  def dot(other: Segment): Double = {
    val v1 = Point2D(this.b.x - this.a.x, this.b.y - this.a.y)
    val v2 = Point2D(other.b.x - other.a.x, other.b.y - other.a.y)
    return (v1.x*v2.x + v1.y*v2.y)
  }

}

case class Ray2D(orig: Point2D, phi: Double)

object SegmentUtils {

  import CommonUtils._
  import PointUtils._

  implicit def _segment2Pair(s: Segment): (Point2D, Point2D) = s.toPair
  implicit def _pair2Segment(ab: (Point2D, Point2D)): Segment = Segment(ab._1, ab._2)
  implicit def _pairPol2Segment(ab: (PointPolar, PointPolar)): Segment = Segment(ab._1, ab._2)

  /**
    * Determines if a point is on a segment
    */
  def pointOnSegment(p: Point2D, s: Segment): Boolean = {
    // p is one of the segment's ends
    if (s.a =~= p || s.b =~= p) return true

    if (crossProduct(s.a, s.b, p) != 0) return false

    val dotprod = dotProduct(s.a, s.b, p)
    if (dotprod < 0) return false

    dotprod <= squaredLengthBA(s.a, s.b)
  }

  def midSegment(p1: Point2D, p2: Point2D) =
    p1 + Direction((p2.x - p1.x) / 2, (p2.y - p1.y) / 2)

  /**
    * Taken from
    * https://www.quora.com/Given-four-Cartesian-coordinates-how-do-I-check-whether-these-two-segments-intersect-or-not-using-C-C++
    */
  def intersect(s1: Segment, s2: Segment): Boolean = {
    if (intersectProper(s1, s2)) {
      return true
    }

    val (a, b) = s1.toPair
    val (c, d) = s2.toPair
    if (pointOnSegment(c, s1) || pointOnSegment(d, s1) ||
        pointOnSegment(a, s2) || pointOnSegment(b, s2)) {
      return true
    }

    false
  }

  def intersectInterior(s1: Segment, s2: Segment): Boolean = {
    intersectProper(s1, s2) && !(s2 contains s1.a) && !(s2 contains s1.b)
  }

  def intersectProper(s1: Segment, s2: Segment) = {
    val (p1, p2) = s1.toPair
    val (p3, p4) = s2.toPair
    val d1 = crossProduct(p3, p4, p1)
    val d2 = crossProduct(p3, p4, p2)
    val d3 = crossProduct(p1, p2, p3)
    val d4 = crossProduct(p1, p2, p4)
    d1 * d2 < 0 && d3 * d4 < 0
  }

  def computeDirectionNormalized(a: Point2D, b: Point2D): Direction = {
    val dx = b.x - a.x
    val dy = b.y - a.y
    if (dx == 0) return Direction(0, 1)
    if (dy == 0) return Direction(1, 0)
    val sq = math.sqrt(dx * dx + dy * dy)
    Direction(dx / sq, dy / sq)
  }

  def intersectSegments(seg1: Segment, seg2: Segment): Option[Point2D] = {
    // seg1 is just one point
    if (seg1.a =~= seg1.b && pointOnSegment(seg1.a, seg2)) {
      return Some(seg1.a)
    }
    // seg2 is just one point
    if (seg2.a =~= seg2.b && pointOnSegment(seg2.a, seg1)) {
      return Some(seg2.a)
    }

    if (seg1.a =~= seg2.a || seg1.a =~= seg2.b) return Some(seg1.a)
    if (seg1.b =~= seg2.a || seg1.b =~= seg2.b) return Some(seg1.b)

    val p = seg1.a
    val r = Direction(seg1.b.x - seg1.a.x, seg1.b.y - seg1.a.y)
    val q = seg2.a
    val s = Direction(seg2.b.x - seg2.a.x, seg2.b.y - seg2.a.y)

    // The segments are collinear
    if ((r ** s) =~= 0.0 && ((q - p) ** r) =~= 0.0) {
      val t0 = ((q - p) * r) / (r * r)
      val t1 = t0 + (s * r) / (r * r)

      lineIntervalIntersection(t0, t1, 0, 1) match {
        case None => return None
        case Some(tt) =>
          val res = p + r.scaleBy(math.max(tt, 0))
          //assert(pointOnSegment(res, seg1), s"Point $res is not on segment $seg1.")
          //assert(pointOnSegment(res, seg2), s"Point $res is not on segment $seg2.")
          return Some(res)
      }
    }

    // Parallel, with no intersection
    if ((r ** s) =~= 0.0 && q ** r =!= 0.0) return None

    // Intersect, so we compute basing on u
    val u = ((q - p) ** r) / (r ** s)
    val t = ((q - p) ** s) / (r ** s)

    if (r ** s =!= 0.0 && 0.0 <=~ u && u <=~ 1.0 && t >=~ 0.0 && t <=~ 1.0) {
      val res = q + s.scaleBy(math.max(u, 0))
      //assert(pointOnSegment(res, seg1), s"Point $res is not on segment $seg1.")
      //assert(pointOnSegment(res, seg2), s"Point $res is not on segment $seg2.")
      return Some(res)
    }

    // Otherwise do not intersect
    None
  }

  /**
    * A variation of the previous method
    */
  def intersectSegmentWithRay(seg: Segment, ray: Ray2D): Option[Point2D] = {
    val phi = ray.phi
    // seg is just one point
    if (seg.a =~= seg.b) {
      val sphi = seg.a.phi
      if (math.cos(sphi) =~= math.cos(phi) && math.sin(sphi) =~= math.sin(phi)) {
        Some(seg.a)
      } else None
    }

    val p = ray.orig
    val r = Direction(math.cos(phi), math.sin(phi))
    val q = seg.a
    val s = Direction(seg.b.x - seg.a.x, seg.b.y - seg.a.y)

    // The segment is on the ray (i.e., they are collinear)
    if ((r ** s) =~= 0.0 && ((q - p) ** r) =~= 0.0) {
      val t0 = ((q - p) * r) / (r * r)
      val t1 = t0 + (s * r) / (r * r)

      val tt = math.max(t0, t1)
      if (tt >=~ 0.0) {
        val res = p + r.scaleBy(math.max(tt, 0))
        assert(pointOnSegment(res, seg), s"Point $res is not on segment $seg.")
        return Some(res)
      } else {
        return None
      }
    }

    // Parallel, with no intersection
    if ((r ** s) =~= 0.0 && q ** r =!= 0.0) return None

    // Intersect, so we compute basing on u
    val u = ((q - p) ** r) / (r ** s)
    val t = ((q - p) ** s) / (r ** s)

    if (r ** s =!= 0.0 && 0.0 <=~ u && u <=~ 1.0 && t >=~ 0.0) {
      val res = q + s.scaleBy(math.max(u, 0))
//      assert(pointOnSegment(res, seg), s"Point $res is not on segment $seg.")
      return Some(res)
    }

    // Otherwise do not intersect
    None
  }

  /**
    * Compute intersection of [a,b] and [c,d]
    */
  def lineIntervalIntersection(a1: Double, b1: Double, c1: Double, d1: Double): Option[Double] = {
    val (a, b) = if (a1 <=~ b1) (a1, b1) else (b1, a1)
    val (c, d) = if (c1 <=~ d1) (c1, d1) else (d1, c1)

    assert(a <=~ b)
    assert(c <=~ d)

    // Full containment
    if (a <=~ c && d <=~ b) return Some(c)
    if (c <=~ a && b <=~ d) return Some(a)

    // Left end
    if (a <=~ c && c <=~ b) return Some(c)
    if (c <=~ a && a <=~ d) return Some(a)

    // Right end
    if (a <=~ d && d <=~ b) return Some(d)
    if (c <=~ b && b <=~ d) return Some(b)

    None
  }

  def segmentsCollinear(s1: Segment, s2: Segment) =
    (s1.direction ** s2.direction) =~= 0.0

}
