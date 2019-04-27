package lambda.geometry.integer

import lambda.geometry.floating.{FPoint, FSegment}

/**
  * A segment with integer coordinates
  */
case class ISegment(a: IPoint, b: IPoint) {

  def squaredLength() =
    (b.x - a.x) * (b.x - a.x) + (b.y - a.y) * (b.y - a.y)

  def dotProduct(other: ISegment): Int =
    (b.x - a.x) * (other.b.x - other.a.x) +
      (b.y - a.y) * (other.b.y - other.a.y)


}
