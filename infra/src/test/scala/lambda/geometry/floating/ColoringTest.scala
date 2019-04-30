package lambda.geometry.floating

import lambda.geometry.floating.examples.FPolygonExamples._
import lambda.geometry.floating.triangles.DualGraphUtils._
import org.scalatest.{FlatSpec, Matchers}

/**
  * @author Ilya Sergey
  */

class ColoringTest extends FlatSpec with Matchers {

  def coloringTest(p: FPolygon): Unit = {
    s"A polygon $p" should s"be colored appropriately" in {
      val colors = constructColoring(p)
      for (z@FSegment(a, b) <- p.edges) {
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


