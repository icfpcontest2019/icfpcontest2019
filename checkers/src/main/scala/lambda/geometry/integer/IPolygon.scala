package lambda.geometry.integer

import lambda.geometry.floating.{FPolygon, FPolygonUtils}

/**
  *
  * A polygon with integer-point coordinates
  *
  * @author Ilya Sergey
  */
case class IPolygon(vertices: Seq[IPoint]) {

  def getVertices: List[IPoint] = vertices.toList

  def toFPolygon : FPolygon = FPolygon(vertices.map(_.toFPoint))

  def isWellFormed = {
    /* TODO:
     * - Check if left-sided
     *
     */

    FPolygonUtils.noSelfIntersections(toFPolygon)
  }

  /*
  TODO:

  * Check if rectilinear
  * Check if left-sided

   */

}
