package lambda.misc.rendering

import java.awt.{Color, Graphics}

import lambda.geometry.{Point2D, Polygon, RenderUtils, Segment}
import lambda.geometry.RenderUtils.DrawingPanel
import lambda.geometry.generators.{MapGeneratorInstance, RandomEasyPolygonGenerator, RandomPointGenerator}
import lambda.geometry.generators.{MapGeneratorInstance, RandomPointGenerator}
import lambda.geometry.{Point2D, Polygon, RenderUtils, Segment}

object MapGenerator {
  class MapPanel(polygons: Seq[Polygon], randomPoints: Seq[Point2D]) extends DrawingPanel {
    override def paint(graphics: Graphics): Unit = {
      super.paint(graphics)
    }

    override def polygonsToDraw(): Seq[(Polygon, Color)] = {
      for ((poly, i) <- polygons.zipWithIndex)
        yield (poly, COLORS(i % COLORS.length))
    }

    override def pointsToDraw(): Seq[(Point2D, Color)] = {
      for (point <- randomPoints)
        yield (point, Color.black)
    }

    override def segmentsToDraw(): Seq[(Segment, Color)] = List()

    override def polygonBordersToDraw(): Seq[(Polygon, Color)] = List()
  }

  def main(args: Array[String]): Unit = {
    val generator = new MapGeneratorInstance(RandomEasyPolygonGenerator, 5, 12)

    val pointGenerator = new RandomPointGenerator(400)

    generator.generate().sample match {
      case None => println("Unable to generate a sample map.")
      case Some(polygons) => {
        println(polygons.length)

        var (topLeft, bottomRight) = RenderUtils.getPolygonBoundingBox(polygons)
        val randomPoints = pointGenerator.generateOutsidePolygons(topLeft, bottomRight, polygons)

        RenderUtils.display(new MapPanel(polygons, randomPoints))
      }
    }
  }
}