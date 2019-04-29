package lambda.geometry.floating.triangles

import lambda.geometry.Turn
import lambda.geometry.floating.FPointUtils._
import lambda.geometry.floating.SegmentUtils._
import lambda.geometry.floating.{FPoint, FPolygon, FSegment}

/**
  * @author Ilya Sergey
  */

class Triangle(val v1: FPoint, val v2: FPoint, val v3: FPoint) extends FPolygon(Seq(v1, v2, v3)) {

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

    val s1 = FSegment(v1, mid23)
    val s2 = FSegment(v2, mid31)
    val s3 = FSegment(v3, mid12)

    val p = intersectSegments(s1, s3)
    assert(p.isDefined)
    assert(containsPoint(p.get))
    p.get
  }

  def oppositeVertex(e: FSegment): FPoint = {
    assert(edges.exists(_ =~= e))
    if (e =~= FSegment(v1, v2)) return v3
    if (e =~= FSegment(v2, v3)) return v1
    assert(e =~= FSegment(v3, v1))
    v2
  }

}

