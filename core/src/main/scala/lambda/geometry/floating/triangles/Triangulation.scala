package lambda.geometry.floating.triangles

import lambda.geometry._
import lambda.geometry.floating.SegmentUtils._
import lambda.geometry.floating.{FPoint, FPolygon, FSegment}

/**
  * Triangulations of simple polygons
  *
  * @author Ilya Sergey
  */

object Triangulation {

  /**
    * A naive quadratic triangulation algorithm
    */
  def triangulate(pol: FPolygon): Set[Triangle] = {
    val pol1 = FPolygon(rotateSeqNumLoop(pol.vertices, pol.vertices.size / 3))

    val vs = pol1.getVertices
    if (vs.size <= 2) return Set.empty
    if (vs.size == 3) return Set(mkTriangle(vs))

    val (tr, newPol) = findEar(pol1)
    Set(tr) ++ triangulate(newPol)
  }

  def mkTriangle(vs: Seq[FPoint]) =
    new Triangle(vs.head, vs(1), vs(2)).canonical

  def isEar(pol: FPolygon, candidate: (FPoint, FPoint, FPoint)): Boolean = {
    val (a, b, c) = candidate
    val es = pol.edges
    val cand = FSegment(a, c)
    val vs = pol.vertices.toList
    val c1 = pol.containsPointProper(cand.middle)
    val c2 = !es.exists(e => e =~= cand || e.flip =~= cand)
    val c3 = vs.forall(v => v =~= cand.a || v =~= cand.b || !cand.contains(v))
    // [bug] happens if to change to intersect
    val c4 = es.forall(e => !intersectProper(cand, e))
    c1 && c2 && c3 && c4
  }

  def findEar(pol: FPolygon): (Triangle, FPolygon) = {
    // A polygon always have an year
    val vs = pol.vertices.toList
    val ear@(a, b, c) = (vs.head, vs.tail.head, vs.tail.tail.head)
    if (isEar(pol, ear)) {
      val tr = mkTriangle(Seq(a, b, c))
      val newPol = FPolygon(vs.drop(2) ++ Seq(a))
      (tr, newPol)
    } else {
      val newPol = FPolygon(rotateSeqNum(vs, 1))
      findEar(newPol)
    }

  }

}

