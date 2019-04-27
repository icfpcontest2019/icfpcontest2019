package lambda.geometry

import CommonUtils._
import PointUtils._
import SegmentUtils._
import org.apache.commons.math3.linear._


/**
  * A generic implementation of a polygon, implemented as a list of its own
  * vertices (ordered counter-clockwise). Therefore, it also admits
  * definition of degenerated polygons.
  *
  * @author Ilya Sergey
  *
  */

case class Polygon(vertices: Seq[Point2D]) extends EpsEq[Polygon] {

  def getVertices: List[Point2D] = vertices.toList

  override def toString = {
    vertices.map(_.toString).mkString(", ")
  }

  def =~=(q: Polygon): Boolean = {
    val vs = q.vertices
    if (vs.size != vertices.size) return false

    // Check the rotations of vertice sequence
    for (i <- 0 to vs.size) {
      if (seqEpsEq(vertices, rotateSeqNum(vs, i))) return true
    }

    false
  }

  def edges: Seq[Segment] =
    PolygonUtils.getEdges(vertices).map { case (a, b) => Segment(a, b) }

  def rotateCWAndShift(phi: Double, newOrigin: Point2D): Polygon = {
    assert(vertices.size >= 3)
    val tmpOrigin = vertices.head
    val shiftedToOrigin = vertices.map(_ - tmpOrigin)
    // rotate clockwise
    val rotated = shiftedToOrigin.map(_.rotateClockWise(phi))
    val shiftedToDest = rotated.map(_ + newOrigin)
    // Remove artifacts of rotation
    val res = Polygon(shiftedToDest).prettifyAlmostIntVertices
    res
  }

  // TODO : Remove debug prints
  def checkIfSamePolygon(other: Polygon): Boolean = {
    if (this.vertices.size != other.vertices.size) {
      //println("NoMap: Not the same number of vertices")
      return false
    }

    // Compute the centroids of both polygons
    val x0 = this.vertices.foldLeft(0.0)(_ + _.x) / this.vertices.size
    val y0 = this.vertices.foldLeft(0.0)(_ + _.y) / this.vertices.size
    val x1 = other.vertices.foldLeft(0.0)(_ + _.x) / this.vertices.size
    val y1 = other.vertices.foldLeft(0.0)(_ + _.y) / this.vertices.size

    // Create matrices with the first 2 points
    var li = 0
    while ((this.vertices(li).x - x0) * (this.vertices(li + 1).y - y0) =~= (this.vertices(li + 1).x - x0) * (this.vertices(li).y - y0)) {
      li += 1
    } //non-zero determinant necessary for LU decomposition
    val V0 = new Array2DRowRealMatrix(Array(Array(this.vertices(li).x - x0, this.vertices(li).y - y0),
      Array(this.vertices(li + 1).x - x0, this.vertices(li + 1).y - y0)))

    val V1 = new Array2DRowRealMatrix(Array(Array(other.vertices(li).x - x1, other.vertices(li).y - y1),
      Array(other.vertices(li + 1).x - x1, other.vertices(li + 1).y - y1)))

    // Linear System is : V0 * R = V1 and we want to find R
    val solver = new LUDecomposition(V0).getSolver()
    val T = new Array2DRowRealMatrix(2, 2)
    try {
      val R = solver.solve(V1)

      //println(R)

      // Check that R.Rt = I
      val I = R.transpose().multiply(R)
      if (((I.getEntry(0, 0) - 1).abs > Eps) || ((I.getEntry(1, 0) - 0).abs > Eps) || ((I.getEntry(0, 1) - 0).abs > Eps) || ((I.getEntry(1, 1) - 1).abs > Eps)) {
        //println("NoMap: Transformation is not a rotation matrix")
        return false
      }

      // Additional test to prevent flipping! Test det R = 1 (and not -1 which would be flipped)
      if ((R.getEntry(0, 0) * R.getEntry(1, 1) - R.getEntry(1, 0) * R.getEntry(0, 1) - 1).abs > Eps) {
        //println("Flipped polygon!")
        return false
      }

      T.setEntry(0, 0, R.getEntry(0, 0)) // T is the transpose of R
      T.setEntry(0, 1, R.getEntry(1, 0))
      T.setEntry(1, 0, R.getEntry(0, 1))
      T.setEntry(1, 1, R.getEntry(1, 1))

    } catch {
      case s: SingularMatrixException => return false
      case _: Throwable => print("Something went wrong in the LU decomposition"); return false
    }

    // Check the rotation applies to all vertices
    for (i <- 2 until this.vertices.size) {
      val P0 = new Array2DRowRealMatrix(2, 1)
      P0.setEntry(0, 0, this.vertices(i).x - x0)
      P0.setEntry(1, 0, this.vertices(i).y - y0)
      val P1 = T.multiply(P0)

      /*      println("TEST PRINTS")
            println(P0)
            println(P1)
            println(other.vertices(i).x -x1)
            println(other.vertices(i).y -y1) */

      if ((P1.getEntry(0, 0) =!= other.vertices(i).x - x1) || (P1.getEntry(1, 0) =!= other.vertices(i).y - y1)) {
        //println("NoMap: There is no rotation that maps the points from the source polygon to the destination")
        return false
      }
    }
    true
  }

  def stretch(k: Double): Polygon = {
    assert(k > 0)
    Polygon(vertices.map(p => Point2D(p.x * k, p.y * k)))
  }

  def prettifyAlmostIntVertices = Polygon(vertices.map(roundPoint))

  def containsPoint(p: Point2D) = PolygonUtils.pointInPolygon(this, p)

  def containsPointProper(p: Point2D) =
    containsPoint(p) && !edges.exists(edge => edge contains p)

  /**
    * @return true is this polygon is convex
    */
  def isConvex = PolygonUtils.checkConvex(vertices)

  /**
    * Move polygon pol so x would become its origin
    */
  def shiftToOrigin(x: Point2D) = Polygon(vertices.map(v => v - x))

  /**
    * Remove aligned vertices from a polygon boundary and return a new polygon
    */
  def removeAligned: Polygon = {
    val n = vertices.size
    val triplesInd =
      (for (k <- 0 to n - 3) yield (k, k + 1, k + 2)) ++
        Seq((n - 2, n - 1, 0), (n - 1, 0, 1))
    val triplesV = triplesInd.map { case (i, i1, i2) => (vertices(i), vertices(i1), vertices(i2)) }

    val fs = triplesV.filter { case (a, b, c) => crossProduct(a, b, c) =!= 0.0 }
    Polygon(fs.map(_._2))
  }

  def contains(other: Polygon): Boolean = {
    // A polygon contains a second polygon if and only if their edges do not intersect,
    // and all of the vertices of the second polygon are contained (or on the edges) in the first polygon.
    val cond1 = !(this edgeIntersectInterior other)
    val cond2 = other.vertices.forall(v => this.containsPoint(v))
    cond1 && cond2
  }

  def edgeIntersectInterior(other: Polygon): Boolean = {
    // Polygons are not contained if (but not only if) edges intersect without any vertex contained on the edges
    edges.exists(edgeLeft => other.edges.exists { edgeRight =>
      edgeLeft intersectInterior edgeRight
    })
  }

  def edgeIntersect(other: Polygon): Boolean = {
    // Polygons intersect edges if and only if two edges intersect
    edges.exists(edgeLeft => other.edges.exists(edgeRight => edgeLeft intersect edgeRight))
  }

  def intersect(other: Polygon): Boolean = {
    // Polygons intersect if and only if
    // (1) either polygon is contained inside the other, or
    // (2) their edges intersect
    (this edgeIntersect other) || (this contains other)
  }

  def overlap(other: Polygon): Boolean = {
    // Polygons overlap if and only if
    // (1) either polygon is contained inside the other, or
    // (2) their edges intersect properly
    (this edgeIntersectInterior other) || (other edgeIntersectInterior this) || (this contains other) || (other contains this) ||
      (this.vertices.exists(v => other.containsPointProper(v)) && this.vertices.exists(v => !other.containsPointProper(v))) ||
      (other.vertices.exists(v => this.containsPointProper(v)) && other.vertices.exists(v => !this.containsPointProper(v)))
  }

  def intersect(other: Segment): Boolean = {
    // A polygon intersects a segment if and only if
    // (1) either point is inside the polygon, or
    // (2) an edge of the polygon intersects the segment
    containsPoint(other.a) || containsPoint(other.b) || edges.exists(edge => edge intersect other)
  }

  def intersectProper(other: Segment): Boolean = {
    // If the segment is entirely contained on an edge, there is no proper intersection.
    if (edges.exists(edge => edge.contains(other))) {
      false
    } else {
      // First, find out the edges that intersect the segment.
      val crossedEdges = edges.filter(edge => edge.intersect(other))

      // Now find out the exact crossings of those edges.
      var crossings = other.a :: other.b :: Nil

      for (crossedEdge <- crossedEdges) {
        if (crossedEdge collinear other) {
          if (other.contains(crossedEdge)) {
            // Edge is entirely contained in the segment; two crossings of interest.
            crossings = crossedEdge.a :: crossedEdge.b :: crossings
          } else {
            // Edge overlaps with the segment, find out which end overlaps and add that.
            if (other.contains(crossedEdge.a)) {
              crossings = crossedEdge.a :: crossings
            } else {
              crossings = crossedEdge.b :: crossings
            }
          }
        } else {
          // Edge and segment are not collinear, so only one intersection.
          crossings = other.intersection(crossedEdge).get :: crossings
        }
      }

      // Find out the subsegments, the interior of which is either entirely contained in the polygon
      // or entirely outside of the polygon.
      val subsegments = crossings
        .sortBy(crossing => crossing.distanceTo(other.a))
        .sliding(2).map(pair => Segment(pair.head, pair.tail.head))

      // The segment now properly intersects the polygon if one of its subsegments does.
      subsegments.exists(segment => containsPointProper(segment.middle))
    }
  }

  def area: Double = PolygonUtils.computeArea(this)

  def isASquare: Boolean = {
    if (vertices.size != 4) return false
    for (e <- 0 until edges.size - 1) { //check sides are orthogonal and same length
      if ((edges(e).length - edges(e + 1).length).abs > 10 * Eps) return false
      if (edges(e).dot(edges(e + 1)).abs > 10 * Eps) return false
    }
    true
  }
}

/**
  * Util functions for polygons
  */
object PolygonUtils {

  // [WTF] Somehow more type-safe structural subtyping doesn't work here, so I just use AnyVal
  implicit def _points2Poly(ps: Seq[(AnyVal, AnyVal)]): Polygon = {

    // Crude but efficient and simple
    val ps1 = ps.map { case (a, b) =>
      (a.asInstanceOf[ToDouble].toDouble, b.asInstanceOf[ToDouble].toDouble)
    }
    Polygon(ps1.map { case (a, b) => Point2D(a, b) })
  }

  /**
    * Checking convexity of a polygon
    */
  def checkConvex(p: Polygon): Boolean = checkConvex(p.vertices)

  def checkConvex(vs: Seq[Point2D]): Boolean = {
    // Checking by implementing a z-crossproduct algorithm

    if (vs.size <= 2) return true

    def pos(f: Double) = f > 0

    val n = vs.size
    val triplesInd =
      (for (k <- 0 to n - 3) yield (k, k + 1, k + 2)) ++
        Seq((n - 2, n - 1, 0), (n - 1, 0, 1))

    val triplesV = triplesInd.map { case (i, i1, i2) => (vs(i), vs(i1), vs(i2)) }
    val zps = triplesV.map { case (a, b, c) => crossProduct(a, b, c) }

    val allPos = zps.forall(pos)
    val allNeg = zps.forall(x => !pos(x))

    allPos || allNeg
  }

  def getEdges[T](vs: Seq[T]): Seq[(T, T)] =
    if (vs.size <= 1) Nil
    else {
      val n = vs.size
      (for (i <- 1 until n) yield (vs(i - 1), vs(i))) ++ Seq((vs(n - 1), vs.head))
    }


  /**
    * Check if a point is inside of a polygon
    */
  def pointInPolygon(pn: Polygon, pt: Point2D): Boolean = {
    val vs = pn.vertices

    // Trivial cases
    if (vs.size == 1) return vs.head =~= pt
    if (vs.size == 2) return pointOnSegment(pt, (vs.head, vs(1)))

    // Check the boundary
    for (e <- pn.edges) {
      if (pointOnSegment(pt, e)) {
        return true
      }
    }

    val convenientAngle = {
      val edgeAngles = for {
        Segment(Point2D(x1, y1), Point2D(x2, y2)) <- pn.edges
        dx = x2 - x1
        dy = y2 - y1
        angle = math.atan2(dy, dx)
      } yield angle

      val verticeAngles = for {
        Point2D(x1, y1) <- pn.vertices
        Point2D(x2, y2) = pt
        dx = x1 - x2
        dy = y1 - y2
        angle = math.atan2(dy, dx)
      } yield angle

      val n = pn.vertices.size * 2 + 1
      val candidates = for (i <- 0 to n) yield i * PI / n


      val candidates1 = candidates.filterNot(c => edgeAngles.exists(a => a =~= c))
      val candidates2 = candidates1.filterNot(c => verticeAngles.exists(a => a =~= c))

      candidates2.head
    }

    val ray = Ray2D(pt, convenientAngle)

    val count = pn.edges.count(e => intersectSegmentWithRay(e, ray).isDefined)

    count % 2 == 1
  }

  private def shrinkUniformly(p: Polygon, k: Double): Polygon = {
    assert(k > 0)
    val vs = for (v <- p.vertices) yield Point2D(v.x / k, v.y / k)
    Polygon(vs).prettifyAlmostIntVertices
  }

  def rotateToEdge(p: Polygon, e: Segment): Polygon = {
    // e is really an edge of p
    assert(p.edges.contains(e))
    // shift to e.b
    // rotate so former [a, b] would be on a negative axis and b would be (0, 0)
    val vs = p.shiftToOrigin(e.b).vertices.map(_.rotateClockWise(e.angle).toCart)
    // make (0, 0) to be the first one
    val rt = rotateSeq(vs, origin2D)._1
    Polygon(rt).prettifyAlmostIntVertices
  }

  def edgeIsConvex(p: Polygon, e: Segment): Boolean = {
    val rotated = rotateToEdge(p, e)
    for (v <- rotated.vertices) {
      if (v.y <~ 0.0) return false
    }
    true
  }

  def findShortestConvexEdge(p: Polygon): Option[Segment] = {
    val convexE = p.edges.filter(edgeIsConvex(p, _))
    if (convexE.isEmpty) return None
    val h = convexE.head
    val t = convexE.tail
    // Find the shortest edge
    val res = t.foldLeft(h)((r, e) => if (r.length <= e.length) r else e)
    Some(res)
  }

  def prepareForAttachment(p: Polygon, vertK: Double): Option[Polygon] = {
    val er = findShortestConvexEdge(p)
    if (er.isEmpty) return None
    val e = er.get
    val k = e.length
    // Make the attachment edge 1
    val p1 = shrinkUniformly(rotateToEdge(p, e), k)
    // Stretch vertically
    //val vs = for (v <- p1.vertices) yield Point2D(v.x, v.y * k)
    val vs = p1.vertices
    Some(Polygon(vs))
  }

  def computeArea(p: Polygon): Double = {
    // Computes the area of an arbitrary polygon with the shoelace formula
    var area = 0.0
    for (idx <- 0 until p.vertices.length - 1) {
      val p1 = p.vertices(idx)
      val p2 = p.vertices(idx + 1)
      area = area + 0.5 * (p1.x * p2.y - p2.x * p1.y)
    }
    area = area + 0.5 * (p.vertices.last.x * p.vertices.head.y - p.vertices.head.x * p.vertices.last.y)
    area = area.abs
    area
  }

  def noSelfIntersections(p: Polygon): Boolean = {
    val es = p.edges
    for {
      e <- es
      g <- es
      if !(e =~= g)
    } {
      if (e.intersectProper(g)) return false
      val inter = intersectSegments(e, g)
      if (inter.isDefined) {
        val p = inter.get
        if (segmentsCollinear(e, g)) return false
      }
    }
    true

  }

}