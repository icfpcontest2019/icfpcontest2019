package lambda.geometry.floating.visibility

import lambda.geometry.GeometryException
import lambda.geometry.floating.SegmentUtils._
import lambda.geometry.floating.triangles.{Triangle, Triangulation}
import lambda.geometry.floating.visibility.JoeSimpsonVisibility._
import lambda.geometry.floating.{FPoint, FPolygon, FSegment}

/**
  * Checking visibility of a set of points in a polygon
  *
  * @author Ilya Sergey
  */

object VisibilityChecker {

  object FailReason extends Enumeration {
    type FailReason = Value
    val Outside, NotVisible = Value

  }
  import FailReason._

  def toStringWithPoint(fr: FailReason, p: FPoint) = fr match {
    case Outside => s"Point $p is outside of the polygon."
    // [FIX] Suggested by Joshua: do not expose the counterexample
    case NotVisible => s"There are points within the polygon, which are *not* visible from your set of guards."
  }

  /**
    * The boolean part is the actual result. The option contains a point if the result is false, which is not
    * in any of the visibility areas (hence the result is false in this case) or is outside.
    *
    * It also returns the final triangle partition
    */
  def checkVisibility(pol: FPolygon, points: Seq[FPoint]): (Boolean, Option[(FPoint, FailReason)], Set[Triangle]) = {

    // Initial check that all points are inside of the polygon
    for (p <- points) {
      if (!pol.containsPoint(p)) return (false, Some(p, Outside), Set.empty)
    }

    /* Main procedure */

    // Compute all visibility polygons
    val vps = for (p <- points;
                   vp = visibilityPolygon(pol, p)
                   if vp.isDefined) yield vp.get
    // Initial triangulation
    val init_ts = Triangulation.triangulate(pol)

    // Construct all triangles, each of which is either fully visible or fully invisible (proved by induction)
    val all_ts = vps.foldLeft(init_ts)(splitTriangleByPolygons)

    //    println(s"All resulting triangles for $pol:")
    //    for (y <- all_ts) {
    //      println(y)
    //    }

    // Take all centers of the triangles
    val centers = all_ts.map(_.center)
    // Check that each center is in the initial polygon
    //assert(centers.forall(pol.containsPoint))

    // Find out if there is a center, which is invisible
    for (c <- centers) {
      // No visibility polygon contains it
      if (!vps.exists(_.containsPoint(c))) return (false, Some(c, NotVisible), all_ts)
    }

    (true, None, all_ts)

  }

  private def splitTriangleByPolygons(ts: Set[Triangle], p: FPolygon): Set[Triangle] =
    ts.flatMap(splitTriangleByPolygon(_, p))


  /**
    * Partition the triangle based on intersection points with a polygon
    *
    * An assumption is the polygon vertices are never properly within triangle vertices
    */
  def splitTriangleByPolygon(t: Triangle, vp: FPolygon): Set[Triangle] = {

    // Expensive checking that no vp's vertices are actually properly within the triangle t
    // val b = vp.vertices.forall(v => !t.containsPoint(v) || t.edges.exists(pointOnSegment(v, _)))

    // Split affected triangles (progressibely)
    def splitTriangleSet(ts: Set[Triangle], p: FPoint) = {
      ts.flatMap(t =>
        // p is on some edge of t, but not its vertex
        if (t.edges.exists(pointOnSegment(p, _)) && t.vertices.forall(_ =!= p))
          splitTriangleByEdgePoint(t, p)
        else Set(t))
    }

    // Split progressively for all edges of vp
    val result = vp.edges.foldLeft(Set(t))(splitTriangleBySegments)
    result
  }

  private def splitTriangleBySegments(ts: Set[Triangle], s: FSegment): Set[Triangle] =
    ts.flatMap(splitTriangleBySegment(_, s))

  /**
    * Split the triangle, possibly intersected by the segment
    */
  def splitTriangleBySegment(t: Triangle, s: FSegment): Set[Triangle] = {

    val intersectionPoints =
      for (e <- t.edges;
           r = intersectSegments(e, s)
           // The intersection exists
           if r.isDefined;
           p = r.get
           // s intersect in the middle
           if p =!= e.a && p =!= e.b) yield p
    intersectionPoints.foldLeft(Set(t))(splitTriangleSet)
  }

  // Split affected triangles (progressibely)
  private def splitTriangleSet(ts: Set[Triangle], p: FPoint) = {
    ts.flatMap(t =>
      // p is on some edge of t, but not its vertex
      if (t.edges.exists(pointOnSegment(p, _)) && t.vertices.forall(_ =!= p))
        splitTriangleByEdgePoint(t, p)
      else Set(t))
  }

  def splitTriangleByEdgePoint(t: Triangle, p: FPoint): Set[Triangle] = {
    assert(t.vertices.forall(_ =!= p), s"$p should not be a vertex")
    val er = t.edges.find(e => pointOnSegment(p, e))
    assert(er.isDefined)
    val e = er.get
    val v = t.oppositeVertex(e)
    val t1 = new Triangle(v, p, e.b).canonical
    val t2 = new Triangle(e.a, p, v).canonical
    Set(t1, t2)
  }

  def isInvisible(pol: FPolygon, guards: Seq[FPoint], thief: FPoint) : (Boolean, String) = {

    if (!pol.containsPoint(thief)) {
      return (false, s"Point $thief is outside of the polygon.")
    }

    if (guards.exists(!pol.containsPoint(_))) {
      val g = guards.find(!pol.containsPoint(_))
      throw new GeometryException("checkIfInvisible",
        s"Something is wrong with the input polygon\n$pol: the guard ${g.get} is not inside of it")
    }

    // Visibility polygon
    val vpsr = guards.map(g => (g, visibilityPolygon(pol, g)))
    if (vpsr.exists(_._2.isEmpty)) {
      throw new GeometryException("checkIfInvisible",
        s"Something is wrong with the input polygon\n$pol: some visibility polygons cannot be computed")
    }

    val vps = vpsr.map{case (a, b) => (a, b.get)}

    val res = vps.forall(vp => !vp._2.containsPoint(thief))
    if (res) return (true, "OK")

    val v = vps.find(vp => vp._2.containsPoint(thief))
    assert(v.isDefined)

    val (g, vp) =  v.get

    (false, s"Point $thief is visible from some guard.")

  }



}
