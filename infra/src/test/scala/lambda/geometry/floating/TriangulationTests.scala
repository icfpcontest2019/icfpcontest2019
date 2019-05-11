package lambda.geometry.floating

import lambda.geometry.floating.examples.FPolygonExamples._
import lambda.geometry.floating.generators.CompositePolygon
import lambda.geometry.floating.generators.PolygonCombinatorUtils._
import lambda.geometry.floating.generators.old.RandomCrazyPolygonGenerator
import lambda.geometry.floating.triangles.Triangulation
import lambda.geometry.floating.triangles.Triangulation._
import org.scalacheck.Prop._
import org.scalacheck.Properties
import org.scalatest.prop.Checkers
import org.scalatest.{FlatSpec, FunSuite, Matchers}


/**
  * @author Ilya Sergey
  */

class TriangulationTests extends FlatSpec with Matchers {

  def triangulationTest(p: FPolygon, perv: Boolean = false, n: Int = 0): Unit = {
    assert(FPolygonUtils.noSelfIntersections(p))

    s"A polygon $p" should s"be triangulated correctly" in {
      val ts = triangulate(p)
      val sz = ts.size
      // assert(sz == m, s"The size is in fact $sz.")
      //      println(s"Initial polygon: $p\nTriangles:")
      for (t <- ts) {
        // Check centers and vertices of triangles
        // println(t)
        val c = t.center
        assert(p.containsPoint(c), s"$p doesn't contain $c.")
        for (v <- t.vertices) {
          assert(p.vertices.exists(_ =~= v), s"$p doesn't contain a vertex $v of $t")
        }
      }
    }
  }

  triangulationTest(weirdRectPolygon)
  triangulationTest(lShapedPolygon)
  triangulationTest(convexPoly)
  triangulationTest(trianglePoly)
  triangulationTest(simpleNonConvexPoly)
  triangulationTest(kittyPolygon)
  triangulationTest(simpleStarPolygon)
  triangulationTest(tHorror)
  triangulationTest(triangBug)


}

object TriangulationSpecification extends Properties("Triangulation") {

  import lambda.geometry.floating.generators.PolygonPropertyUtils._

  import RandomCrazyPolygonGenerator._

  val triangulationInside = forAll { (p: CompositePolygon) =>
    collect(polygonCombinatorCollector(p)) {
      //println(s"Testing polygon $p")
      val trs = Triangulation.triangulate(p)
      trs.forall(t => p.containsPoint(t.center))
    }
  }

  val triangulationCount = forAll { (p: CompositePolygon) =>
    collect(polygonCombinatorCollector(p)) {
      //println(s"Testing polygon $p")
      val trs = Triangulation.triangulate(p)
      trs.size == p.size - 2
    }
  }

  // property("Center of each triangle lies within a polygon") = triangulationInside
  // property("A number of (possibly degenerate) triangles is n − 2") = triangulationCount
}

