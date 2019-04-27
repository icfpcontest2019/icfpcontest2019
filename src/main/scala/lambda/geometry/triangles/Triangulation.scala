package lambda.geometry.triangles

import lambda.geometry.{CommonUtils, Point2D, Polygon, Segment}
import lambda.geometry.PointUtils._
import lambda.geometry.SegmentUtils._
import lambda.geometry.{Point2D, Polygon, Segment, Turn}
import lambda.geometry._

/**
  * Triangulations of simple polygons
  *
  * @author Ilya Sergey
  */

object Triangulation {

  /**
    * A naive quadratic triangulation algorithm
    */
  def triangulate(pol: Polygon): Set[Triangle] = {
    val pol1 = Polygon(CommonUtils.rotateSeqNumLoop(pol.vertices, pol.vertices.size / 3))

    val vs = pol1.getVertices
    if (vs.size <= 2) return Set.empty
    if (vs.size == 3) return Set(mkTriangle(vs))

    val (tr, newPol) = findEar(pol1)
    Set(tr) ++ triangulate(newPol)
  }

  def mkTriangle(vs: Seq[Point2D]) =
    new Triangle(vs.head, vs(1), vs(2)).canonical

  def isEar(pol: Polygon, candidate: (Point2D, Point2D, Point2D)): Boolean = {
    val (a, b, c) = candidate
    val es = pol.edges
    val cand = Segment(a, c)
    val vs = pol.vertices.toList
    val c1 = pol.containsPointProper(cand.middle)
    val c2 = !es.exists(e => e =~= cand || e.flip =~= cand)
    val c3 = vs.forall(v => v =~= cand.a || v =~= cand.b || !cand.contains(v))
    // [bug] happens if to change to intersect
    val c4 = es.forall(e => !intersectProper(cand, e))
    c1 && c2 && c3 && c4
  }

  def findEar(pol: Polygon): (Triangle, Polygon) = {
    // A polygon always have an year
    val vs = pol.vertices.toList
    val ear@(a, b, c) = (vs.head, vs.tail.head, vs.tail.tail.head)
    if (isEar(pol, ear)) {
      val tr = mkTriangle(Seq(a, b, c))
      val newPol = Polygon(vs.drop(2) ++ Seq(a))
      (tr, newPol)
    } else {
      val newPol = Polygon(CommonUtils.rotateSeqNum(vs, 1))
      findEar(newPol)
    }

  }

}

