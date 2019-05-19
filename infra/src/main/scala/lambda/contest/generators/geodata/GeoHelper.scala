package lambda.contest.generators.geodata

import java.awt.{BorderLayout, Color, Graphics}

import geotrellis.vector.io.json.{GeoJson, JsonFeatureCollection}
import geotrellis.vector.{Point, Polygon}
import javax.swing.{BoxLayout, JFrame, JPanel}
import lambda.contest.generators.PolygonToRender
import lambda.geometry.floating.RenderUtils._
import lambda.geometry.floating.{FPoint, FPolygon, FPolygonUtils, FSegment}
import lambda.geometry.integer.{IPoint, IntersectionUtils}

/**
  * @author Ilya Sergey
  */
object GeoHelper {

  import geotrellis.vector.io._

  def polyToFPoly(p: Polygon): FPolygon = {
    val vertices = p.vertices.toList.map((pt: Point) => FPoint(pt.x, pt.y))
    FPolygon(vertices)
  }

  def main(args: Array[String]): Unit = {

    val geoPath = args(0)
    val boxSize = 199

    val centered: FPolygon = getCountryScaled(geoPath, boxSize)

    renderCountry(centered)
    
    //val json = collection.toJson
    //val list = json.asJsObject.getFields("features")
    //val aruba = list(0)
  }

  def getCountryScaled(geoPath: String, boxSize: Int) = {
    val collection = GeoJson.fromFile[JsonFeatureCollection](geoPath)
    val polys = collection.getAllPolygons().toList.map(polyToFPoly)

    val largest = if (geoPath.contains("denmark")) {
      polys.sortBy(FPolygonUtils.computeArea).reverse(1)
    } else polys.maxBy(FPolygonUtils.computeArea)

    rescaleCountry(boxSize, largest)
  }

  def rescaleCountry(boxSize: Int, poly: FPolygon) = {
    // println(largest)
    val bottomLeft = getPolygonBoundingBox(poly)._1
    val shifted = poly.shiftToOrigin(bottomLeft)
    val (_, FPoint(dx, dy)) = getPolygonBoundingBox(shifted)

    // println(s"Dimensions: ${(dx, dy)}")
    val maxDim = math.max(dx, dy)
    val scaleFactor = boxSize / maxDim
    val countryScaled = shifted.stretch(scaleFactor)
    // println(s"Scaled BB: ${getPolygonBoundingBox(countryScaled)}")
    val (_, FPoint(fx, fy)) = getPolygonBoundingBox(countryScaled)
    val centered = countryScaled.shiftToOrigin(FPoint(fx / 2, fy / 2))


    val xyRatio = math.min(2.5, fx / fy)
    val scaledAndCentered = FPolygon(centered.vertices.map { case FPoint(x, y) => FPoint(x, y * xyRatio) })
    val (_, FPoint(dx1, dy1)) = getPolygonBoundingBox(scaledAndCentered)
    val maxDim1 = math.max(dx1, dy1)
    val scaleFactor1 = boxSize / maxDim1
    val countryScaled1 = scaledAndCentered.stretch(scaleFactor1)

    val finalPoly = countryScaled1.removeZeroEdges

    val (lower, _) = getPolygonBoundingBox(finalPoly)
    finalPoly.shiftToOrigin(lower)
  }

  private def renderCountry(largest: _root_.lambda.geometry.floating.FPolygon) = {
    val pp = PolygonToRender(largest)

    val frame = new JFrame()
    val polygonPanel = new JPanel() {
      override def paint(g: Graphics) = {
        pp.fillWhiteBackground(g)
        pp.fillPoly(g, pp.polygon, Color.LIGHT_GRAY)

        val (FPoint(x1, y1), FPoint(x2, y2)) = getPolygonBoundingBox(pp.polygon)

        for {e@FSegment(a, b) <- pp.polygon.edges
             IPoint(i, j) <- IntersectionUtils.cellsCrossedBySegment(e)
             square = FPolygon(List(FPoint(i, j), FPoint(i + 1, j),
               FPoint(i + 1, j + 1), FPoint(i, j + 1)))
        } {
          pp.fillPoly(g, square, Color.BLUE)
        }


        //        for {i <- x1.toInt to x2.toInt
        //             j <- y1.toInt to y2.toInt} {
        //          
        //          val square = FPolygon(List(FPoint(i, j), FPoint(i + 1, j),
        //            FPoint(i + 1, j + 1), FPoint(i, j + 1)))
        //          if (j == 0) {
        //            println(s"Done processing: ${((i - x1) / (x2 - x1) * 100).toInt}%")
        //          }
        //          if (pp.polygon.contains(square)) {
        //            pp.fillPoly(g, square, Color.BLUE)
        //          }
        //        }

      }
    }

    frame.setLayout(new BoxLayout(frame.getContentPane, BoxLayout.Y_AXIS))
    frame.add(polygonPanel, BorderLayout.NORTH)
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    val size = pp.frameSize
    frame.setSize(size._1, size._2)
    frame.setVisible(true)
  }
}
