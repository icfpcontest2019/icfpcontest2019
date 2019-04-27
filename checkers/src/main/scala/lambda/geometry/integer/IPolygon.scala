package lambda.geometry.integer

import lambda.geometry.floating.{FPolygon, FPolygonUtils}
import lambda.geometry.getEdges

/**
  *
  * A polygon with integer-point coordinates
  *
  * @author Ilya Sergey
  */
case class IPolygon(vertices: Seq[IPoint]) {

  def edges: Seq[ISegment] =
    getEdges(vertices).map { case (a, b) => ISegment(a, b) }


  def toFPolygon: FPolygon = FPolygon(vertices.map(_.toFPoint))

  def isRectilinear: Boolean = {
    val es = edges
    if (es.isEmpty) return false
    val esPaired = es.zip(es.tail ++ Seq(es.head))
    esPaired.forall{case (e1, e2) => e1.dotProduct(e2) == 0}
  }

  def isWellFormed = {
    /* TODO:
     * - Check if left-sided
     *
     */
    val nonZeroEdges = edges.forall(e => e.squaredLength > 0)

    val noIntersect = FPolygonUtils.noSelfIntersections(toFPolygon)
    nonZeroEdges && noIntersect
  }

  /*
  TODO:

  * Check if rectilinear
  * Check if left-sided

   */

}
