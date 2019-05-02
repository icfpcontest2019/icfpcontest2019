package lambda.contest

import lambda.geometry.floating.{FPoint, FSegment}
import lambda.geometry.integer.IPoint
import lambda.geometry.integer.IntersectionUtils._
import org.scalatest.{FlatSpec, Matchers}

/**
  * @author Ilya Sergey
  */
class DiscreteVisibilityTests extends FlatSpec with Matchers {

  val p1 = FPoint(0.5, 0.5)
  val p2 = FPoint(2.5, 2.5)
  val p3 = FPoint(1.5, 2.5)

  val seg1 = FSegment(p1, p2)
  val seg2 = FSegment(p2, p1)
  val seg3 = FSegment(p1, p3)
  val seg4 = FSegment(p3, p1)

  val cell = IPoint(1, 1)
  val cell2 = IPoint(0, 1)


  s"The discrete segment/cell checker" should "work correctly" in {
    assert(segmentIntersectsCell(seg1, cell))
  }

  it should "handle reversed segment fine" in {
    assert(segmentIntersectsCell(seg2, cell))
  }

  it should "intersections by edges" in {
    assert(segmentIntersectsCell(seg3, cell))
    assert(segmentIntersectsCell(seg4, cell))
  }

  it should "intersections by edges again" in {
    assert(segmentIntersectsCell(seg3, cell2))
    assert(segmentIntersectsCell(seg4, cell2))
  }

  it should "identify touchings correctly" in {
    assert(!segmentIntersectsCell(seg1, cell2))
    assert(!segmentIntersectsCell(seg2, cell2))
  }

}
