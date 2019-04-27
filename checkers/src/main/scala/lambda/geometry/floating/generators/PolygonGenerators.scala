package lambda.geometry.floating.generators

import lambda.geometry._
import lambda.geometry.floating.PointUtils._
import lambda.geometry.floating.SegmentUtils._
import lambda.geometry.floating._
import lambda.geometry.floating.generators.PolygonCombinatorUtils._
import org.scalacheck.Gen

/**
  * @author Ilya Sergey
  */


/**
  * Generation of simple rectilinear polygons
  */
object PolygonGenerators {

  // Get orientation of the outside-going normal ray for the edge
  def getOrientation(s: FSegment): Double = s.angle

  private def intersectsExisting(e: FSegment, pol: FPolygon, attached: FPolygon): Boolean = {
    val vs = attached.vertices
    if (vs.size < 3) return false
    val es = for (i <- 0 until vs.size - 1) yield FSegment(vs(i), vs(i + 1))

    val aPoint = vs.head
    val bPoint = vs.last
    // (1) Check that all non-adjacent edges don't intersect the original polygon
    for (g <- pol.edges if g =!= e && g.b =!= aPoint && g.a =!= bPoint) {
      // Intersect some non-adjacent edges -- bad!
      if (es.exists(s => intersect(g, s))) return true
    }

    // (2) Now check if no nodes of the attached are inside
    for (av <- vs if av =!= vs.head && av =!= vs.last) {
      if (pol.containsPoint(av)) return true
    }

    // Check (1) and (2) should suffice to ensure the absence of collision

    false
  }

  private def splitAndAttach(e: FSegment, pc: CompositePolygon,
                             attached: FPolygon): Option[CompositePolygon] = {
    // Check whether these are good segments
    if (intersectsExisting(e, pc, attached)) return None

    Some(Attached(pc, e, attached))
  }

  /**
    * Get a leaning angle and an attachment point of an edge
    */
  def getEdgeParams(e: FSegment, posStrategy: Double => Option[(Int, Int)]): Option[(Double, FPoint, Int)] = {
    val l = e.length
    if (l < 1) return None
    val phi = e.angle
    val FSegment(a, _) = e
    // get an attachment point on the edge 1 unit away from the further end
    val offsets = posStrategy(l)
    if (offsets.isEmpty) return None
    val (startOffset, endOffset) = offsets.get
    assert(startOffset >= 0)
    assert(endOffset <= l)
    val stretchK = endOffset - startOffset
    assert(stretchK >= 1)
    val aPoint = a + PointPolar(startOffset, phi).toCart
    Some((phi, aPoint, stretchK))
  }

  def generatePolygon(base: FPolygon, toAttach: Gen[LazyPolygon], polSize: Gen[Int],
                      n: Int, posStrategy: Double => Option[(Int, Int)]) =
    extendPolygonIter(BasePolygon(base), toAttach, polSize, n, posStrategy)

  private def extendPolygon(pc: CompositePolygon, attachments: Gen[LazyPolygon],
                            polSize: Gen[Int], posStrategy: Double => Option[(Int, Int)]): CompositePolygon = {
    val es = randomRotation(pc.edges)
    for (e <- es) {
      val size = polSize.sample.get
      val attached = attachments.sample.get(size)
      val params = getEdgeParams(e, posStrategy)
      if (params.isDefined) {
        val (phi, a, str) = params.get
        val adjusted = attached.stretch(str).rotateCWAndShift(PI - phi, a)
        val np = splitAndAttach(e, pc, adjusted)
        if (np.isDefined) {
          val pol = np.get
          if (FPolygonUtils.noSelfIntersections(pol)) return pol
        }
      }
      // Tries to split while doesn't succeed
    }
    pc
  }

  type LazyPolygon = (Int) => FPolygon

  def prep(p: FPolygon): LazyPolygon = (d: Int) => {
    val eSize = randomIntBetween(1, d)
    val r = FPolygonUtils.prepareForAttachment(p, eSize)
    assert(r.isDefined)
    r.get
  }

  def prepNoScale(p: FPolygon): LazyPolygon = (d: Int) => {
    val r = FPolygonUtils.prepareForAttachment(p, 1)
    assert(r.isDefined)
    r.get
  }

  /**
    * Extends a rectilinear polygon with appendices n times
    *
    * @param init        Initial polygon
    * @param attachments randomly generated new appendices
    * @param n           number of iterations
    * @return new polygon
    */
  def extendPolygonIter(init: CompositePolygon, attachments: Gen[LazyPolygon], polSize: Gen[Int],
                        n: Int, posStrategy: Double => Option[(Int, Int)]): CompositePolygon = {

    // Just iterate extension n times
    val base: CompositePolygon = generators.BasePolygon(init)
    (0 until n).foldLeft(base)((p, _) => extendPolygon(p, attachments, polSize, posStrategy))
  }


}
