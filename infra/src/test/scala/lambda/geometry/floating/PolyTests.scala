package lambda.geometry.floating

import lambda.geometry.floating.SegmentUtils.pointOnSegment
import lambda.geometry.floating.examples.FPolygonExamples._
import org.scalatest.{FlatSpec, Matchers, MustMatchers}


/**
  * @author ilya
  */

class PolyTests extends FlatSpec with Matchers {


  s"A triangle $trianglePoly" should "be identified as convex polygon" in {
    assert(trianglePoly.isConvex)
  }

  s"A convex $convexPoly" should "be identified as such" in {
    assert(convexPoly.isConvex)
  }

  s"A convex $convexPoly2" should "be identified as such" in {
    assert(convexPoly2.isConvex)
  }

  s"A convex $convexPoly3" should "be identified as such" in {
    assert(convexPoly3.isConvex)
  }

  s"A non-convex $simpleNonConvexPoly" should "be identified as such" in {
    assert(!simpleNonConvexPoly.isConvex)
  }

  s"A non-convex $nonConvexPoly5" should "be identified as such" in {
    assert(!nonConvexPoly5.isConvex)
  }

}

/**
  * Testing points being on a segment
  */
class PointSegmentTests extends FlatSpec with MustMatchers {

  val (s1, p1) = (FSegment(FPoint(-1, -1), FPoint(1, 1)), FPoint(0, 0))
  val (s2, p2) = (FSegment(FPoint(-1, -1), FPoint(1, 1)), FPoint(0, 0.001))
  val (s3, p3) = (FSegment(FPoint(-1, -1), FPoint(1, 1)), FPoint(2, 2))

  s"Point $p1" should s"be in segment $s1" in {
    assert(pointOnSegment(p1, s1))
  }

  s"Point $p2" should s"be not in segment $s2" in {
    assert(!pointOnSegment(p2, s2))
  }

  s"Point $p3" should s"be not in segment $s3" in {
    assert(!pointOnSegment(p3, s3))
  }

}


/**
  * Testing whether a point is in a polygon
  */
class PointInPolygonTests extends FlatSpec with MustMatchers {

  val p1 = FPoint(1, 1)
  val p2 = FPoint(1, 0.5)
  val p3 = FPoint(2, 3)
  val p4 = FPoint(0.5, 0.5)

  s"Point ${(0.5, 1)}" should s"be in polygon $convexPoly4" in {
    assert(convexPoly4.containsPoint(FPoint(0.5, 1)))
  }

  s"Point $p4" should s"be in rectangle$convexPoly3" in {
    assert(convexPoly3.containsPoint(p4))
  }

  s"Point $p1" should s"be in triangle $trianglePoly" in {
    assert(trianglePoly.containsPoint(p1))
    assert(trianglePoly.containsPoint(FPointUtils.origin2D))
    assert(trianglePoly.containsPoint(FPoint(2, 2)))
  }

  s"Point $p2" should s"be in triangle $trianglePoly" in {
    assert(trianglePoly.containsPoint(p2))
  }

  s"Point $p3" should s"not be in triangle $trianglePoly" in {
    assert(!trianglePoly.containsPoint(p3))
  }

  s"Point $p2" should s"be in rectangle$convexPoly" in {
    assert(convexPoly.containsPoint(p2))
  }


  s"Point ${(1, 0)}" should s"be in rectangle$convexPoly3" in {
    assert(convexPoly3.containsPoint(FPoint(1, 0)))
  }

  s"Point $p4" should s"not be in rectangle$simpleNonConvexPoly" in {
    assert(!simpleNonConvexPoly.containsPoint(p4))
  }

}
