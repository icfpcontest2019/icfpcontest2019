package lambda.geometry

import lambda.geometry.SegmentUtils._
import lambda.geometry.examples.UsefulPolygons._
import lambda.geometry.generators.{MapGeneratorInstance, RandomPointGenerator, RandomRectilinearPolygonGenerator}
import org.scalatest.{FlatSpec, Matchers, MustMatchers}

import scala.util.Random


/**
  * @author Ilya Sergey
  */

class SegmentIntersectionCheckTests extends FlatSpec with MustMatchers {

  def checkIntersect(s1: Segment, s2: Segment, exp: Boolean, proper: Boolean = false): Unit = {
    s"$s1 and $s2" should (if (exp) "" else "not " + "intersect") in {
      val res = if (proper) intersectProper(s1, s2) else intersect(s1, s2)
      if (!proper) {
        assert(!intersectProper(s1, s2), s"Segments $s1 and $s2 cannot intersect properly")
      }
      assert(res == exp, s"The result of intersection of $s1 and $s2 shoudl be $exp")
    }
  }

  val s1 = (Point2D(-1, -1), Point2D(1, 1))
  val s2 = (Point2D(-1, 1), Point2D(1, -1))
  val s3 = (Point2D(-1, -1), Point2D(-2, -2))
  val s4 = (Point2D(0, 0), Point2D(0, 4))
  val s5 = (Point2D(-1, 0), Point2D(1, 2))
  val s6 = (Point2D(-3, 3), Point2D(-4, 6))

  checkIntersect(s1, s2, exp = true, proper = true)
  checkIntersect(s1, s3, exp = true)
  checkIntersect(s1, s4, exp = true)
  checkIntersect(s1, s5, exp = false)
  checkIntersect(s1, s6, exp = false)

}

class SegmentsIntersectionTests extends FlatSpec with MustMatchers {

  def checkIntersectSegments(z1: Segment, z2: Segment, myres: Option[Point2D] = None, exp: Boolean = true): Unit = {
    s"$z1 and $z2" should (if (exp) "" else "not " + s"intersect") in {
      val res = intersectSegments(z1, z2)
      // println(res)
      assert(res.isDefined == exp, s"The result of intersection of $z1 and $z2 should be $exp")

      if (res.isDefined) {
        assert(res.get =~= myres.get)
      }
    }
  }


  val s1 = (Point2D(-1, -1), Point2D(1, 1))
  val s2 = (Point2D(0, 1), Point2D(1, 0))
  val s3 = (Point2D(0, 1), Point2D(0, 2))
  val s4 = (Point2D(-1, -1), Point2D(1, -2))
  val s5 = (Point2D(-1, 1), Point2D(1, 1))
  val s6 = (Point2D(0, 2), Point2D(1, 0))
  val s7 = (Point2D(0, 0), Point2D(2, 2))
  val s8 = (Point2D(0, 1), Point2D(2, 3))
  val s9 = (Point2D(-1, -1), Point2D(-0.01, -0.01))

  checkIntersectSegments(s1, s2, Some(Point2D(0.5, 0.5)))
  checkIntersectSegments(s1, s7, Some(Point2D(0, 0)))
  checkIntersectSegments(s2, s3, Some(Point2D(0, 1)))
  checkIntersectSegments(s4, s6, exp = false)
  checkIntersectSegments(s3, s5, Some(Point2D(0, 1)))
  checkIntersectSegments(s7, s8, exp = false)
  checkIntersectSegments(s7, s9, exp = false)

}

class SegmentRayIntersectionTests extends FlatSpec with MustMatchers {

  def checkIntersectRay(s: Segment, ray: Ray2D, myres: Option[Point2D] = None, exp: Boolean = true): Unit = {
    s"Segments $s" should (if (exp) "" else "not " + s"intersect the ray $ray") in {
      val res = intersectSegmentWithRay(s, ray)
      assert(res.isDefined == exp, s"The result of intersection of $s and $ray shoudl be $exp")

      if (res.isDefined) {
        assert(res.get =~= myres.get)
      }
    }
  }

  val s1 = (Point2D(-1, -1), Point2D(1, 1))
  val s2 = (Point2D(0, 1), Point2D(1, 0))
  val s3 = (Point2D(0, 1), Point2D(1, 2))
  val s4 = (Point2D(-1, -1), Point2D(1, -2))
  val s5 = (Point2D(-1, 1), Point2D(1, 1))
  val s6 = (Point2D(0, 2), Point2D(1, 0))
  val s7 = (Point2D(-2, -2), Point2D(-1, -1))
  val s8 = (Point2D(-2, -2), Point2D(0.1, 0.1))

  checkIntersectRay(s1, ray1, Some(Point2D(1, 1)))
  checkIntersectRay(s2, ray1, Some(Point2D(0.5, 0.5)))
  checkIntersectRay(s3, ray1, exp = false)
  checkIntersectRay(s4, ray1, exp = false)
  checkIntersectRay(s5, ray2, Some(Point2D(0, 1)))
  checkIntersectRay(s6, ray3, Some(Point2D(0, 2)))
  checkIntersectRay(s7, ray1, exp = false)
  checkIntersectRay(s8, ray1, Some(Point2D(0.1, 0.1)))

}

class SegmentPolygonProperIntersectionCheckTests extends FlatSpec with Matchers {
  val generator = new MapGeneratorInstance(RandomRectilinearPolygonGenerator, 1, 1)
  val pointGenerator = new RandomPointGenerator(2)

  var done = 0
  while(done < 100) {
    val obstacle = generator.generate().sample.get.head
    var (topLeft, bottomRight) = RenderUtils.getPolygonBoundingBox(obstacle :: Nil)

    val points = pointGenerator.generateOutsidePolygons(topLeft, bottomRight, obstacle :: Nil)
    val segment = Segment(points.head, points.tail.head)
    val random = new Random()

    if (!obstacle.intersectProper(segment)) {
      val point = segment.a + segment.direction.scaleBy(random.nextDouble())

      done += 1
      "Point" should s"not be inside polygon, attempt $done" in {
        assert(!(obstacle containsPointProper point))
      }
    }
  }
}