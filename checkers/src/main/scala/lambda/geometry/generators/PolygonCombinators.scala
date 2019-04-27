package lambda.geometry.generators

import lambda.geometry._
import PolygonCombinatorUtils._
import lambda.geometry.{Point2D, Polygon, Segment}


/**
  * @author Ilya Sergey
  */

/**
  * Abstract class representing a combinator
  */
sealed abstract class CompositePolygon {

  /**
    * Generate polygon
    */
  def pol: Polygon

  def size = pol.vertices.size

  override def toString = pol.toString

  // Flatten to list
  def toList: List[CompositePolygon]


  private def getAllAttachments(cp: CompositePolygon): Set[Polygon] = cp match {
    case BasePolygon(pol) => Set(pol)
    case Attached(b, _, a) => getAllAttachments(b) + a
  }

  def getAttachments: Set[Polygon] = getAllAttachments(this)
}

/**
  * Basic polygon
  */
case class BasePolygon(pol: Polygon) extends CompositePolygon {
  override def toList = List(this)
}

/**
  * Represent the new complex polygon obtained by attaching the polygon "attached"
  * to the edge of "base".
  *
  * It also keeps the reference to the parent combinator to maintain the backwards tree structure
  */
case class Attached(base: CompositePolygon, e: Segment, attached: Polygon) extends CompositePolygon {

  override def pol: Polygon = {
    val vs = base.vertices
    val aPoint = e.a
    val bPoint = e.b
    // locate the beginning of the edge
    val i = vs.indexWhere(_ =~= aPoint)
    assert(i >= 0, s"Node $aPoint is not in polygon $base.")
    val newVs = vs.take(i + 1) ++ trimOverlappingEnds(attached.vertices, aPoint, bPoint) ++ vs.drop(i + 1)
    Polygon(newVs)
  }

  def trimOverlappingEnds(vs: Seq[Point2D], aPoint: Point2D, bPoint: Point2D): Seq[Point2D] = {
    assert(vs.size >= 3)
    // trim head if necessary
    val vs1 = if (vs.head =~= aPoint) vs.tail else vs
    // trim last element if necessary
    val vs2 = if (vs1.last =~= bPoint) vs1.take(vs1.size - 1) else vs1
    vs2
  }

  lazy val parent: CompositePolygon = computeParent

  private def computeParent: CompositePolygon = {

    // Unroll the structure recursively
    def computeLoop(pc: CompositePolygon): CompositePolygon =
      pc match {
        case z@BasePolygon(pol) => z
        case z@Attached(b, _, a) =>
          // the attachment of a created our connection edge e
          // (either by splitting an existing edge or introducing a new one)
          if (z.edges.exists(_ =~= e) &&
              !b.edges.exists(_ =~= e)) {
            z
          } else computeLoop(b)
      }

    val res = computeLoop(base)
    assert(res.pol.edges.exists(_ =~= e))
    res
  }

  override def toList = this :: base.toList
}


