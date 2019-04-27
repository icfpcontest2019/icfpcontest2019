package lambda.misc.rendering

import java.awt.{Color, Graphics}

import lambda.geometry.floating.RenderUtils.DrawingPanel
import lambda.geometry.floating.generators.{MapGeneratorInstance, RandomEasyPolygonGenerator, RandomPointGenerator}
import lambda.geometry.floating.{FPoint, FPolygon, RenderUtils, FSegment}

object MapGenerator {
  class MapPanel(polygons: Seq[FPolygon], randomPoints: Seq[FPoint]) extends DrawingPanel {
    override def paint(graphics: Graphics): Unit = {
      super.paint(graphics)
    }

    override def polygonsToDraw(): Seq[(FPolygon, Color)] = {
      for ((poly, i) <- polygons.zipWithIndex)
        yield (poly, COLORS(i % COLORS.length))
    }

    override def pointsToDraw(): Seq[(FPoint, Color)] = {
      for (point <- randomPoints)
        yield (point, Color.black)
    }

    override def segmentsToDraw(): Seq[(FSegment, Color)] = List()

    override def polygonBordersToDraw(): Seq[(FPolygon, Color)] = List()
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