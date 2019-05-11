package lambda.geometry.floating.generators

import lambda.geometry.floating.FPolygonUtils.{noSelfIntersections, prepareForAttachment}
import lambda.geometry.floating.SegmentUtils.intersect
import lambda.geometry.floating.{Direction, FPoint, FPolygon, FPolygonUtils, FSegment, PI, PointPolar, generators}
import lambda.geometry.{randomIntBetween, randomRotation}
import org.scalacheck.Gen

/**
  * Generation of simple rectilinear polygons
  */
object PolygonGenerators {

  type LazyPolygon = Int => FPolygon
  type PolygonCondition = FPolygon => Boolean
  type AttachmentStrategy = (Double, FPolygon) => Option[(Int, Int, Int)]

  def generatePolygon(base: FPolygon,
                      toAttach: Gen[(LazyPolygon, AttachmentStrategy)],
                      cond: PolygonCondition,
                      polSize: Gen[Int],
                      n: Int) =
    extendPolygonIter(BasePolygon(base), toAttach, cond, polSize, n)

  /**
    * Extends a rectilinear polygon with appendices n times
    *
    * @param init        Initial polygon
    * @param attachments randomly generated new appendices with position strategy
    * @param polSize     Sizes for lazy polygons 
    * @param n           number of iterations
    * @return new polygon
    */
  def extendPolygonIter(init: CompositePolygon,
                        attachments: Gen[(LazyPolygon, AttachmentStrategy)],
                        cond: PolygonCondition,
                        polSize: Gen[Int],
                        n: Int): CompositePolygon = {

    // Just iterate extension n times
    var p: CompositePolygon = generators.BasePolygon(init.pol)
    var p1 = p
    var i = 0
    do {
      p1 = p
      println(s"Iteration: $i / $n")
      p = extendPolygon(p, attachments, cond, polSize)
      i = i + 1
    } while (i <= n && p1.size < p.size)
    p
  }

  private def extendPolygon(pc: CompositePolygon,
                            attachments: Gen[(LazyPolygon, AttachmentStrategy)],
                            cond: PolygonCondition,
                            polSize: Gen[Int]): CompositePolygon = {
    //    val es = randomRotation(splitMaxEdges(pc))
    val (es1, es2) = splitMaxEdges(pc)
    val es = randomRotation(es1)
    for (e <- es ++ es2) {
      val size = polSize.sample.get
      val (attached, posStrategy) = attachments.sample.get
      val polyToAttach = attached(size)
      val params = getEdgeParams(e, polyToAttach, posStrategy)
      if (params.isDefined) {
        val (phi, a, str) = params.get
        val adjusted = polyToAttach.stretch(str).rotateCWAndShift(PI - phi, a)
        splitAndAttach(e, pc, adjusted) match {
          case Some(cp) =>
            val pol = cp.pol
            if (noSelfIntersections(pol) && cond(pol)) {
              val ipol = pol.toIPolygon
              val area = FPolygonUtils.computeArea(pol)
              println(s"${ipol.vertices.size} vertices, area: ${area.toInt}")
              
              return cp
            }
          case None =>
        }
      }
      // Tries to split while doesn't succeed
    }
    pc
  }

  def prep(p: FPolygon): LazyPolygon = (d: Int) => {
    val eSize = randomIntBetween(1, d)
    val r = prepareForAttachment(p, eSize)
    assert(r.isDefined)
    r.get
  }

  def prepNoScale(p: FPolygon): LazyPolygon = (_: Int) => {
    val r = prepareForAttachment(p, 1)
    assert(r.isDefined)
    r.get
  }


  /* -------------------------------------------------------------------- */
  /*                                  Utility methods                     */
  /* -------------------------------------------------------------------- */


  // Get orientation of the outside-going normal ray for the edge
  private def getOrientation(s: FSegment): Double = s.angle

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
    // Check whether these are no existing segments
    if (intersectsExisting(e, pc.pol, attached)) return None

    Some(Attached(pc, e, attached))
  }

  /**
    * Get a leaning angle and an attachment point of an edge
    */
  private def getEdgeParams(e: FSegment,
                            p: FPolygon,
                            posStrategy: AttachmentStrategy):
  Option[(Double, FPoint, Int)] = {
    val l = e.length
    if (l < 1) return None
    val phi = e.angle
    val FSegment(a, _) = e
    // get an attachment point on the edge 1 unit away from the further end
    val offsets = posStrategy(l, p)
    if (offsets.isEmpty) return None
    val (startOffset, endOffset, scalingFactor) = offsets.get
    assert(startOffset >= 0)
    assert(endOffset <= l)
    assert(scalingFactor >= 1)
    val z = PointPolar(startOffset, phi).toCart
    val aPoint = a + Direction(z.x, z.y)
    Some((phi, aPoint, scalingFactor))
  }

  private def splitMaxEdges(pc: CompositePolygon): (Seq[FSegment], Seq[FSegment]) = {
    val es = pc.pol.edges
    val eMax = es.maxBy(_.length).length
    val eMin = es.minBy(_.length).length
    val mid = (eMax - eMin) / 2
    (es.filter(_.length >= mid), es.filter(_.length < mid))

  }


}
