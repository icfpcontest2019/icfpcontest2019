package lambda.geometry

import org.scalatest.{FlatSpec, Matchers}
import lambda.geometry.examples.UsefulPolygons._
import lambda.geometry.triangles.DualGraphUtils._

/**
  * @author Ilya Sergey
  */

class ColoringTest extends FlatSpec with Matchers {

  def coloringTest(p: Polygon): Unit = {
    s"A polygon $p" should s"be colored appropriately" in {
      val colors = constructColoring(p)
      for (z@Segment(a, b) <- p.edges) {
        val c1 = colors(a)
        val c2 = colors(b)
//        println(s"Coloring of edge $z: $c1, $c2")
        assert(c1 != c2, s"Bad coloring of edge $z: $c1, $c2")
      }
//      println()
    }
  }

  coloringTest(lShapedPolygon)
  coloringTest(kittyPolygon)
  coloringTest(simpleStarPolygon)
  coloringTest(shurikenPolygon)
  coloringTest(polarBug)
  coloringTest(tHorror)
  coloringTest(triangBug)
  coloringTest(strangeKey)
}


