package lambda.geometry.triangles

import lambda.geometry.{Point2D, Polygon, Segment, Turn}
import lambda.geometry.PointUtils._
import lambda.geometry.SegmentUtils._
import lambda.geometry.{Polygon, Segment, Turn}

/**
  * @author Ilya Sergey
  */

class Triangle(val v1: Point2D, val v2: Point2D, val v3: Point2D) extends Polygon(Seq(v1, v2, v3)) {

  import lambda.geometry.Turn._

  def isDegenerate = direction(v1, v2, v3) == NoTurn

  /**
    * Set correct direction (interior on the left)
    */
  def canonical =
    if (direction(v1, v2, v3) != Turn.RightTurn) this
    else new Triangle(v1, v3, v2)

  // one possible center of a triangle
  val center = {
    val mid12 = midSegment(v1, v2)
    val mid23 = midSegment(v2, v3)
    val mid31 = midSegment(v3, v1)

    val s1 = Segment(v1, mid23)
    val s2 = Segment(v2, mid31)
    val s3 = Segment(v3, mid12)

    val p = intersectSegments(s1, s3)
    assert(p.isDefined)
    assert(containsPoint(p.get))
    p.get
  }

  def oppositeVertex(e: Segment): Point2D = {
    assert(edges.exists(_ =~= e))
    if (e =~= Segment(v1, v2)) return v3
    if (e =~= Segment(v2, v3)) return v1
    assert(e =~= Segment(v3, v1))
    v2
  }

}

