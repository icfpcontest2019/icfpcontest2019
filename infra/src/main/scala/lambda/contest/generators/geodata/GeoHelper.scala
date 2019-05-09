package lambda.contest.generators.geodata

import java.awt.{BorderLayout, Color, Graphics}

import geotrellis.vector.{MultiPolygon, Point, Polygon}
import geotrellis.vector.io.json.{GeoJson, JsonFeatureCollection}
import javax.swing.{BoxLayout, JFrame, JPanel}
import lambda.contest.generators.PolygonToRender
import lambda.geometry.floating.RenderUtils._
import lambda.geometry.floating.{FPoint, FPolygon, FPolygonUtils, RenderUtils}

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
    val boxSize = 200

    val centered: FPolygon = getCountryScaled(geoPath, boxSize)
    
    renderCountry(centered)



    //val json = collection.toJson
    //val list = json.asJsObject.getFields("features")
    //val aruba = list(0)
  }

  def getCountryScaled(geoPath: String, boxSize: Int) = {
    val collection = GeoJson.fromFile[JsonFeatureCollection](geoPath)
    val polys = collection.getAllPolygons().toList.map(polyToFPoly)

    val largest = polys.maxBy(FPolygonUtils.computeArea)

    // println(largest)
    val bottomLeft = getPolygonBoundingBox(largest)._1
    val shifted = largest.shiftToOrigin(bottomLeft)
    val (_, FPoint(dx, dy)) = getPolygonBoundingBox(shifted)

    // println(s"Dimensions: ${(dx, dy)}")
    val maxDim = math.max(dx, dy)
    val scaleFactor = boxSize / maxDim
    val countryScaled = shifted.stretch(scaleFactor)
    // println(s"Scaled BB: ${getPolygonBoundingBox(countryScaled)}")
    val (_, FPoint(fx, fy)) = getPolygonBoundingBox(countryScaled)
    val centered = countryScaled.shiftToOrigin(FPoint(fx / 2, fy / 2))
    centered
  }

  private def renderCountry(largest: _root_.lambda.geometry.floating.FPolygon) = {
    val pp = PolygonToRender(largest)

    val frame = new JFrame()
    val polygonPanel = new JPanel() {
      override def paint(g: Graphics) = {
        pp.fillWhiteBackground(g)
        pp.fillPoly(g, pp.polygon, Color.LIGHT_GRAY)
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
