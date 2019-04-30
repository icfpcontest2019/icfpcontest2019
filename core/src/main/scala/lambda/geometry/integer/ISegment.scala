package lambda.geometry.integer

import lambda.geometry.integer.IPointUtils.direction

/**
  * A segment with integer coordinates
  */
case class ISegment(a: IPoint, b: IPoint) {

  def squaredLength() =
    (b.x - a.x) * (b.x - a.x) + (b.y - a.y) * (b.y - a.y)

  def dotProduct(other: ISegment): Int =
    (b.x - a.x) * (other.b.x - other.a.x) +
      (b.y - a.y) * (other.b.y - other.a.y)


  def toPair: (IPoint, IPoint) = (a, b)

  import lambda.geometry.Turn._

  def collinear(s2: ISegment) = {
    val (p1, p2) = toPair
    val (p3, p4) = s2.toPair
    val d1 = direction(p3, p4, p1)
    val d2 = direction(p3, p4, p2)
    d1 == NoTurn && d2 == NoTurn
  }

  /**
    * Precondition:
    * a, b, and p are already on the same line
    */
  def containsPoint(p: IPoint): Boolean = {
    val (a, b) = this.toPair
    if (!ISegment(a, p).collinear(ISegment(p, b))) return false
    val IPoint(ax, ay) = a
    val IPoint(bx, by) = b
    val IPoint(px, py) = p
    math.min(ax, bx) <= px && px <= math.max(ax, bx) &&
    math.min(ay, by) <= py && py <= math.max(ay, by)
  }

  def intersects(s2: ISegment): Boolean = {
    val (p1, p2) = this.toPair
    val (p3, p4) = s2.toPair
    val d1 = direction(p3, p4, p1)
    val d2 = direction(p3, p4, p2)
    val d3 = direction(p1, p2, p3)
    val d4 = direction(p1, p2, p4)
    if (d1 * d2 < 0 && d3 * d4 < 0) return true
    if (d1 == NoTurn && s2.containsPoint(p1)) return true
    if (d2 == NoTurn && s2.containsPoint(p2)) return true
    if (d3 == NoTurn && this.containsPoint(p3)) return true
    if (d4 == NoTurn && this.containsPoint(p4)) return true
    false
  }

}
