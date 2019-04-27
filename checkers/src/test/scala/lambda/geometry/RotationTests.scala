package lambda.geometry

import org.scalatest.{FlatSpec, Matchers}
import CommonUtils._
import lambda.geometry.examples.UsefulPolygons._


/**
  * @author Ilya Sergey
  */

class RotationTests extends FlatSpec with Matchers {

  def rotatePolygon(p: Polygon, k: Int) = {
    val vs: Seq[Point2D] = p.vertices
    val vsPolar = vs.map(_.toPolar).map(_.rotateClockWise(2 * k * PI))
    val vsCart = vsPolar.map(_.toCart)

    for ((v, w) <- vs.zip(vsCart)) {
      assert(v =~= w, s"A new node $w differs too much from the corresponding old node v")
    }
  }

  s"A triangle $trianglePoly" should "when rotated should be equal to itself" in {
    rotatePolygon(trianglePoly, 4)
  }

  s"A polygon $convexPoly" should "when rotated should be equal to itself" in {
    rotatePolygon(convexPoly, 6)
  }

  s"A polygon $convexPoly3" should "when rotated should be equal to itself" in {
    rotatePolygon(convexPoly3, 15)
  }

}

