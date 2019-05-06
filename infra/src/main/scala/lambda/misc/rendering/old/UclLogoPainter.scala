package lambda.misc.rendering.old

import lambda.geometry.floating.triangles.DualGraphUtils
import lambda.geometry.floating.{FPointUtils, FPolygon}
import lambda.misc.rendering.PolygonPainter

/**
  * @author Ilya Sergey
  */
object UclLogoPainter extends PolygonPainter {

  def main(args: Array[String]) {


    val s1 = Seq(
      (100, 0), (100, 10), (90, 15), (90, 25), (-80, 25), (-80, 27),
      // collumns
      (-69, 27), (-69, 95), (-48, 95), (-48, 27),
      (-31, 27), (-31, 95), (-9, 95), (-9, 27),
      (9, 27), (9, 95), (31, 95), (31, 27),
      (48, 27), (48, 95), (69, 95), (69, 27),
      (85, 27), (85, 100),
      (88, 110), (90, 110), (92, 113), (50, 125), (50, 130), (48, 132),
      (48, 140), (45, 147), (43, 147),
      (37, 164), (33, 169), (23, 177), (13, 182),
      (13, 187), (10, 190), (10, 205), (13, 205), (2, 217),
      (3, 218), (3, 223),
      (-3, 223), (-3, 218),
      (-2, 217), (-13, 205), (-10, 205), (-10, 190), (-13, 187),
      (-13, 182), (-23, 177), (-33, 169), (-37, 164),
      (-43, 147), (-45, 147), (-48, 140),
      (-48, 132), (-50, 130), (-50, 125), (-92, 113), (-90, 110), (-88, 110),
      (-85, 100), (-85, 25), (-90, 25), (-90, 15), (-100, 10), (-100, 0))


    val pol = FPolygon(s1.map(FPointUtils._point2D))

    println(pol)
    // println()

    //    val td = Seq(Point2D(0, 195))

    val td = DualGraphUtils.chavatalVisibilitySet(pol)

    //val res = VisibilityChecker.checkVisibility(pol, td)

    //println(res)
    println()
    //println(td)

    drawShape(pol)

  }

}
