package lambda.contest.generators.geodata

import lambda.contest.ContestTask
import lambda.contest.generators.geodata.CountriesToRectilinear.{getLowestStartingPoint, liftedOrigin, walkArrayInDirection}
import lambda.contest.generators.geodata.GeoHelper.rescaleCountry
import lambda.contest.parsers.ContestTaskParser
import lambda.geometry.floating.RenderUtils.getPolygonBoundingBox
import lambda.geometry.floating.{FPoint, FPolygon, FSegment, RenderUtils}
import lambda.geometry.integer.{IPoint, IntersectionUtils}
import lambda.util.FileUtil

/**
  * @author Ilya Sergey
  */
object RenderEnterprise {
  
  
  val enterprise : FPolygon = FPolygon(List((152,26), (151,20), (274,16), (280,21), (368,28), (366,35), (256,43), (316,135), (375,135), (386,128), (406,130), (412,125), (407,117), (432,85), (437,86), (500,63), (559,60), (629,78), (691,114), (690,211), (629,246), (559,262), (500,264), (437,238), (433,239), (408,208), (413,199), (407,196), (386,196), (376,190), (316,187), (256,281), (366,289), (368,295), (280,302), (274,308), (151,315), (152,301), (59,295), (59,289), (232,282), (250,229), (140,225), (140,219), (196,215), (237,179), (186,172), (181,165), (181,161), (186,154), (237,146), (196,108), (140,105), (140,98), (250,96), (232,43), (59,34), (59,29))
    .map{case (x, y) => FPoint(x, y)})


  def main(args: Array[String]): Unit = {
    val (matrix, dx, dy) = shapeArray
    val (x0, y0) = getLowestStartingPoint(matrix, dx, dy)
    val polygon = walkArrayInDirection(matrix, x0, y0)
    assert(polygon.isRectilinear)
    assert(polygon.isWellFormed)
    val pixelEnterprise = polygon.shiftToOrigin
    val IPoint(minx, _) = pixelEnterprise.getMinXY
    val minLeft = pixelEnterprise.vertices.filter(_.x == minx).minBy(_.y)
    println(ContestTask(pixelEnterprise, minLeft, Nil, Nil).toString)

  }
  
  private def shapeArray = {
    val poly = rescaleCountry(99, enterprise)
    val (lowEnd, _) = RenderUtils.getPolygonBoundingBox(poly)
    val polygon = poly.shiftToOrigin(lowEnd).shiftToOrigin(liftedOrigin)
    val (_, FPoint(xr, yr)) = RenderUtils.getPolygonBoundingBox(polygon)

    val dx = (xr + 2.5).toInt
    val dy = (yr + 2.5).toInt
    val matrix = Array.fill(dx)(Array.fill(dy)(0))

    for {e@FSegment(a, b) <- polygon.edges
         IPoint(i, j) <- IntersectionUtils.cellsCrossedBySegment(e)} {
      matrix(i)(j) = 1
    }
    (matrix, dx, dy)

  }
}
