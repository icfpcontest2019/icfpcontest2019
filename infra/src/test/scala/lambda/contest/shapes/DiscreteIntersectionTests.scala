package lambda.contest.shapes

import lambda.geometry.floating.{FPoint, FSegment}
import lambda.geometry.integer.IntersectionUtils.cellsCrossedBySegment
import org.scalatest.{FlatSpec, Matchers}

/**
  * @author Ilya Sergey
  */
class DiscreteIntersectionTests extends FlatSpec with Matchers {

  val s1 = FSegment(FPoint(0.5, 0.5), FPoint(2.5, 2.5))
  intersectionTest(s1, List((0, 0), (1, 1), (2, 2)))

  val s11 = FSegment(FPoint(0.5, 0.5), FPoint(-1.5, -1.5))
  intersectionTest(s11, List((-2, -2), (-1, -1), (0, 0)))

  val s2 = FSegment(FPoint(0.5, 0.5), FPoint(2.5, 0.5))
  intersectionTest(s2, List((0, 0), (1, 0), (2, 0)))

  val s3 = FSegment(FPoint(0.5, 0.5), FPoint(0.5, 3.5))
  intersectionTest(s3, List((0, 0), (0, 1), (0, 2), (0, 3)))

  val s4 = FSegment(FPoint(0.5, 0.5), FPoint(1.5, 2.5))
  intersectionTest(s4, List((0, 0), (0, 1), (1, 1), (1, 2)))

  val s5 = FSegment(FPoint(0.5, 0.5), FPoint(2.5, 4.5))
  intersectionTest(s5, List((0, 0), (0, 1), (1, 1), (1, 2), (1, 3), (2, 3), (2, 4)))

  val s6 = FSegment(FPoint(0.5, 0.5), FPoint(2.5, -1.5))
  intersectionTest(s6, List((0, 0), (1, -1), (2, -2)))

  val s7 = FSegment(FPoint(0.5, 0.5), FPoint(-1.5, 4.5))
  intersectionTest(s7, List((-2, 3), (-2, 4), (-1, 1), (-1, 2), (-1, 3), (0, 0), (0, 1)))

  val s8 = FSegment(FPoint(0.5, 0.5), FPoint(2.5, 1.5))
  intersectionTest(s8, List((0,0), (1,0), (1,1), (2,1)))

  val s9 = FSegment(FPoint(0.5, 0.5), FPoint(-1.5, 1.5))
  intersectionTest(s9, List((-2,1), (-1,0), (-1,1), (0,0)))

  val s10 = FSegment(FPoint(0.5, 0.5), FPoint(4.5, 1.5))
  intersectionTest(s10, List((0,0), (1,0), (2,0), (2,1), (3,1), (4,1)))

  private def intersectionTest(s: FSegment, expected: List[(Int, Int)]) =
    s"The algorithm for determining properly intersected by a segment" should
      s"work correctly for segment $s." in {
      assertResult(expected) {
        cellsCrossedBySegment(s).map(_.toPair)
      }
    }


}
