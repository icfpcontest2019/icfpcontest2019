package lambda.geometry.floating.generators


import lambda.geometry._
import lambda.geometry.floating.SegmentUtils.intersect
import lambda.geometry.floating.generators.PolygonGenerators._
import lambda.geometry.floating.{FPoint, FPolygon, PointPolar, _}
import org.scalacheck.{Gen, Shrink}

/**
  * @author Ilya Sergey
  */

object PolygonPropertyUtils {


  /**
    * Construct an aligned rectangular polygon with the bottom-right
    * angle in the section
    */
  val generateNormalizedRectangle: LazyPolygon =
    (longEdgeSize: Int) => {
      val i = randomIntBetween(1, 1)
      val j = randomIntBetween(2, longEdgeSize)
      FPolygon(Seq(
        FPoint(0, 0), FPoint(0, j),
        FPoint(-i, j), FPoint(-i, 0)))
    }

  val generateNormalizedSquare: LazyPolygon =
    (longEdgeSize: Int) => {
      val i = randomIntBetween(1, 1)
      val j = randomIntBetween(1, 1)
      FPolygon(Seq(
        FPoint(0, 0), FPoint(0, 1),
        FPoint(-1, 1), FPoint(-1, 0)))
    }

  val generate3Rectangle: LazyPolygon =
    (longEdgeSize: Int) =>
      FPolygon(Seq(
        FPoint(0, 0), FPoint(0, longEdgeSize),
        FPoint(-3, longEdgeSize), FPoint(-3, 0)))


  def polyFreqs[T](fs: Seq[Int], ps: Seq[T]): Gen[T] =
    Gen.frequency(fs.zip(ps.map(p => Gen.const(p))): _*)

  val polygonCombinatorCollector = (pc: CompositePolygon) => {
    val n = pc.size
    if (n <= 60) s"Small-size polygon (<= 60 vertices)"
    else if (n > 60 && n <= 150) s"Medium-size polygon (61..150 vertices)"
    else if (n > 150 && n <= 300) s"Large-size polygon (151..300 vertices)"
    else s"Very large-size polygon (>300 vertices)"
  }

  def generateConvexPolygon(n: Int): FPolygon = {
    assert(n > 2)
    val frac = PI2 / n
    val vs: Seq[FPoint] = for (i <- 0 until n; phi = i * frac) yield PointPolar(1, phi).toCart
    floating.FPolygon(vs)
  }

  implicit val shrinkPolygonCombinator: Shrink[CompositePolygon] =
    Shrink { (p: CompositePolygon) =>
      PolygonCombinatorUtils.makeTraversals(p)
    }
}


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
    if (intersectsExisting(e, pc.pol, attached)) return None

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
    val z = PointPolar(startOffset, phi).toCart
    val aPoint = a + Direction(z.x, z.y)
    Some((phi, aPoint, stretchK))
  }
  
  private def degMaxEdges(pc: CompositePolygon)=  {
    val es = pc.pol.edges
    val eMax = es.maxBy(_.length).length
    val eMin = es.minBy(_.length).length
    val mid = (eMax - eMin) / 2
    es.filter(_.length >= mid)
    
  }

  def generatePolygon(base: FPolygon, toAttach: Gen[LazyPolygon], polSize: Gen[Int],
                      n: Int, posStrategy: Double => Option[(Int, Int)]) =
    extendPolygonIter(BasePolygon(base), toAttach, polSize, n, posStrategy)

  private def extendPolygon(pc: CompositePolygon, attachments: Gen[LazyPolygon],
                            polSize: Gen[Int], posStrategy: Double => Option[(Int, Int)]): CompositePolygon = {
    val es = randomRotation(degMaxEdges(pc))
    for (e <- es) {
      val size = polSize.sample.get
      val attached = attachments.sample.get(size)
      val params = getEdgeParams(e, posStrategy)
      if (params.isDefined) {
        val (phi, a, str) = params.get
        val adjusted = attached.stretch(str).rotateCWAndShift(PI - phi, a)
        val np = splitAndAttach(e, pc, adjusted)
        if (np.isDefined) {
          val cp = np.get
          // TODO: Add more checks
          if (FPolygonUtils.noSelfIntersections(cp.pol)) return cp
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
    val base: CompositePolygon = generators.BasePolygon(init.pol)
    (0 until n).foldLeft(base)((p, _) => extendPolygon(p, attachments, polSize, posStrategy))
  }


}
