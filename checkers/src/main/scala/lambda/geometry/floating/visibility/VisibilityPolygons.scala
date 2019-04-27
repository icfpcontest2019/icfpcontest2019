package lambda.geometry.floating.visibility

import lambda.geometry._
import lambda.geometry.floating.PointUtils._
import lambda.geometry.floating.SegmentUtils._
import lambda.geometry.floating.{Direction, FPoint, FPolygon, PointPolar, Ray2D, FSegment, _}

import scala.collection.mutable.{Stack => MStack}

/**
  * Algorithms for computing point-visibility polygons
  *
  * @author Ilya Sergey
  */
object JoeSimpsonVisibility {

  import VisibilityUtil._
  import ScanDir._
  import lambda.geometry.floating.Turn._


  /**
    * Implementation of Joe-Simpson algorithm for a visibility polygon of a point in a simple polygon
    * See the following references:
    * - http://cs.smith.edu/~orourke/books/ArtGalleryTheorems/Art_Gallery_Chapter_8.pdf
    * - https://cs.uwaterloo.ca/research/tr/1985/CS-85-38.pdf
    *
    * This is the main method for computing a visibility area of a polygon pol from a point p,
    */

  def visibilityPolygon(pol: FPolygon, p: FPoint): Option[FPolygon] = {
    // Consider meaningful polygons
    if (pol.vertices.size < 3) {
      return None
    }

    // Ignore the case when the point isn't in the polygon
    if (!pol.containsPoint(p)) {
      return None
    }

    Some(visibilityInternal(pol, p))
  }

  /**
    * Compute visibility polygon of an internal point p in a simple polygon p.
    */
  private def visibilityInternal(pol: FPolygon, z: FPoint): FPolygon = {

    // PROLOGUE - prepare the polygon
    val (vs, initAngle, _) = visibilityPrologue(z, pol)

    val v0 = vs(0)
    // create new stack
    val s0: MStack[VertDispl] = new MStack().push(v0)

    assert(vs.n > 1)

    // MAIN PART
    val s = if (vs(1).alpha >=~ v0.alpha) {
      advance(vs, s0, 0)
    } else {
      scan(vs, s0, 0, None, CounterClockWise)
    }

    // EPILOGUE: Stack s contains the desired points in the reversed order
    // st_0 == vs_0
    assert(s.last.point =~= v0.point)
    // st_t == vs_n-1 -- It seems that the following assertion is wrong
    // assert(s.head.point =~= vs(vs.n - 1).point)
    visibilityEpilogue(s, vs, z, initAngle)
  }

  /**
    * aka PUSH
    */
  def advance(v: VsRep, s: MStack[VertDispl], iprev: Int): Seq[VertDispl] = {
    val n = v.n - 1
    // vs(i + 1) is defined
    assert(iprev + 1 <= n, s"Displacement sequence $v is too short")

    if (v(iprev + 1).alpha <=~ PI2) {
      val i = iprev + 1
      s.push(v(i))
      if (i == n) return s.toSeq

      // Process turns

      if (v(i + 1).alpha <~ v(i).alpha &&
          direction(v(i - 1), v(i), v(i + 1)) == RightTurn) {
        // Process right turn
        scan(v, s, i, None, CounterClockWise)
      } else if (v(i + 1).alpha <~ v(i).alpha &&
          direction(v(i - 1), v(i), v(i + 1)) == LeftTurn) {
        retard(v, s, i)
      } else {
        // Otherwise call itself recursively
        advance(v, s, i)
      }
    } else {
      val v0 = v(0)
      if (s.top.alpha < PI2) {
        val ray = Ray2D(origin2D, v0.phi)
        val isect = intersectSegmentWithRay(FSegment(v(iprev), v(iprev + 1)), ray)
        assert(isect.isDefined, s"Intersection undefined")
        val st = displacementInBetween(isect.get, v(iprev), v(iprev + 1))
        s.push(st)
      }
      scan(v, s, iprev, Some(v0), ClockWise)
    }
  }

  /**
    * aka POP
    */
  def retard(v: VsRep, sold: MStack[VertDispl], iprev: Int): Seq[VertDispl] = {
    val (sj1, sstail) = (sold.top, sold.tail)
    // pop elements from the stack according to the condition
    val (s, sjnext) = locateSj(v(iprev), v(iprev + 1), sj1, sstail)
    val sj = s.top

    if (sj.alpha < v(iprev + 1).alpha) {

      val i = iprev + 1

      val vi = v(i)
      val st1 = intersectSegmentWithRay(floating.FSegment(sj, sjnext), Ray2D(origin2D, vi.phi)).map {
        case p => displacementInBetween(p, sj, sjnext)
      }
      if (st1.isDefined) {
        s.push(st1.get)
      }
      s.push(vi)
      // BUG in the algorithn: suggests i == v.n
      if (i == v.n - 1) {
        s.toSeq
      } else if (v(i + 1).alpha >=~ vi.alpha && direction(v(i - 1), vi, v(i + 1)) == RightTurn) {
        advance(v, s, i)
      } else if (v(i + 1).alpha >~ vi.alpha && direction(v(i - 1), vi, v(i + 1)) == LeftTurn) {
        s.pop()
        scan(v, s, i, Some(vi), ClockWise)
      } else {
        s.pop()
        retard(v, s, i)
      }

    } else {

      if (v(iprev + 1).alpha =~= sj.alpha &&
          // [FIXED bug, thanks to debugging (was just >)]
          v(iprev + 2).alpha >~ v(iprev + 1).alpha &&
          direction(v(iprev), v(iprev + 1), v(iprev + 2)) == RightTurn) {

        s.push(v(iprev + 1))
        advance(v, s, iprev + 1)

      } else {

        val w = intersectWithWindow(v(iprev), v(iprev + 1), sj, Some(sjnext))
        assert(w.isDefined, "This intersection should exist!")
        scan(v, s, iprev, w, CounterClockWise)

      }
    }
  }

  /**
    * aka WAIT
    * @param windowEnd the endpoint of a window, which is either a point (Some(...)) or infinity. The polat angle of a
    *                  window is taken to be the angle of current s.top, as it always lies on the polar ray
    *                  through s.top.
    */
  def scan(v: VsRep, s: MStack[VertDispl], iprev: Int,
           windowEnd: Option[VertDispl], ccw: ScanDir): Seq[VertDispl] = {
    val i = iprev + 1
    /*
    BUG in the original algorithm , discovered by testing
    Without the following check the test [BUG 12-01-16] faithfully fails,
    as scan indeed might be the last operation before returning the result.
     */
    if (i + 1 == v.n) return s
    if (ccw == CounterClockWise &&
        // [BUG 254 fixes, thanks to debugging (was just > instead of >~ in the first clause)]
        // This actually easy to reproduce with VisibilityPolygonsSpecification
        v(i + 1).alpha >~ s.top.alpha &&
        s.top.alpha >=~ v(i).alpha) {
      intersectWithWindow(v(i), v(i + 1), s.top, windowEnd) match {
        // [BUG 25-01-16] -- The following additional check in the case of Some
        // if is totally counterintuitive, but if it's not there
        // the whole computation is screved, as advance proceeds on the wrong premises
        case Some(p) if !(windowEnd.isDefined && p =~= windowEnd.get.toCart) =>
          s.push(p)
          advance(v, s, i)
        case _ =>
          scan(v, s, i, windowEnd, ccw)
      }
    } else if (ccw == ClockWise &&
        v(i + 1).alpha <=~ s.top.alpha &&
        s.top.alpha < v(i).alpha) {
      if (intersectWithWindow(v(i), v(i + 1), s.top, windowEnd).isDefined) {
        retard(v, s, i)
      } else {
        scan(v, s, i, windowEnd, ccw)
      }
    } else {
      scan(v, s, i, windowEnd, ccw)
    }
  }

  /**
    * Prepare the polygon p for processing wrt. a point x within it
    *
    * Returns
    * - the processed representation of the the nodes in the sequence
    * - the initial rotation angle of a polygon -- apply at the end
    * - the number of rotations in the sequence of vertices (to be reverted)
    */
  def visibilityPrologue(z: FPoint, pol: FPolygon): (VsRep, Double, Int) = {
    // Reposition the polygon so x is in the origin
    val shiftedPol = pol.shiftToOrigin(z)

    // z is in fact a vertex of the polygon
    val zIsVertex = shiftedPol.vertices.exists(_ =~= origin2D)

    // Find initial node to start
    val init: PointPolar = getInitVertex(shiftedPol, zIsVertex)

    // Vertices in polar coordinates
    val vsPolOrig: Seq[PointPolar] = shiftedPol.vertices.map(_.toPolar)
    // Make the init node to be the first one and also rotate so it would be a zero
    // This is the sequence of nodes we're going to work with
    val (vs, rotNum) = rotateSeq(vsPolOrig, init)

    // Adjust so the potential origin vertex would be at a proper position
    val vss = if (zIsVertex) Seq(origin2D.toPolar) ++ vs.filter(_ =!= origin2D) else vs

    // now turn so the initial vertex init would be on the horizontal ray
    val vss_clean = vss.map(v => if (v =!= origin2D) v.rotateClockWise(init.phi) else v)

    (new VsRep(vss_clean, zIsVertex), init.phi, rotNum)
  }

  /**
    * Post-process the stack pre_s containing the visibility polygon
    */
  private def visibilityEpilogue(pre_s: Seq[VertDispl], vs: VsRep, z: FPoint, initAngle: Double): FPolygon = {
    // Only add origin if it were a vertex originally, so it was removed from processing
    val s = if (vs.zIsOrigin) Seq(VertDispl(origin2D, 0)) ++ pre_s else pre_s

    // Take the vertices of the resulting polygon in the right order
    val correctOrderVisVertices = s.reverse.map(_.point)
    // Rotate by the inverse angle to get the VP wrt. initial polygon
    val rotatedBackSeq = correctOrderVisVertices.map(_.rotateClockWise(-initAngle).toCart)
    // Shift backwards, so point z would be at its initial location
    val shiftedBack: Seq[FPoint] = rotatedBackSeq.map(_ + Direction(z.x, z.y))

    val res = floating.FPolygon(shiftedBack).removeAligned
    //assert(res.containsPoint(z), s"Visibility area $res contains point $z")
    res
  }

}

object VisibilityUtil {

  /*
   * Representing vertices (in the polar form) with their angular displacements (alpha)
   */
  sealed case class VertDispl(point: PointPolar, alpha: Double)

  implicit def _vd2Polar(v: VertDispl): PointPolar = v.point
  implicit def _vd2Cart(v: VertDispl): FPoint = v.point.toCart

  object ScanDir extends Enumeration {
    type ScanDir = Value
    val ClockWise, CounterClockWise = Value
  }

  /**
    * Find initial vertex, which is visible from the origin
    */
  def getInitVertex(pol: FPolygon, origIsVertex: Boolean): FPoint = {
    val es = pol.edges
    val vs = pol.vertices

    // Take the next one after origin
    if (origIsVertex) {
      val ecand = es.find(e => e.a =~= origin2D)
      assert(ecand.isDefined)
      return ecand.get.b
    }

    // origin is on some edge - return the adjacent vertex
    val e1 = es.find(pointOnSegment(origin2D, _))
    e1 match {
      case Some(e) => return e._2
      case _ =>
    }

    // take vertices, clearly visible from origin
    val visible = vs.filter(v => es.forall(e => !intersectProper((origin2D, v), e)))

    // get the closest.one
    val res = visible.find(v => visible.forall(w => v.rad <=~ w.rad) && v.rad >~ 0.0)

    assert(res.isDefined)
    res.get
  }

  /**
    * Precompute angular displacements for the set of vertices vs
    */
  def angularDisplacementSeq(vs: Seq[PointPolar]): Seq[VertDispl] = {

    import scala.collection.mutable.ArrayBuffer
    val buffer = ArrayBuffer.empty[VertDispl]

    for (i <- vs.indices) {
      if (i == 0) {
        buffer += VertDispl(vs.head, vs.head.phi)
      } else {
        val (vi, viprev) = (vs(i), vs(i - 1))
        val phi = vi.phi
        val rawAngle = math.abs(phi - viprev.phi)

        assert(rawAngle < PI2)

        // An angle is always between 0 and Pi
        val angle = math.min(rawAngle, PI2 - rawAngle)
        val sigma = direction(origin2D, viprev, vi)
        val alpha_vi = buffer(i - 1).alpha + sigma * angle

        assert(checkPeriod(phi, alpha_vi, PI2),
          s"Angular displacements should be equal to the polar angle modulo 2 Pi: $alpha_vi, $phi")

        assert(math.abs(alpha_vi - buffer(i - 1).alpha) < PI, "Angluar displacement cannot be bigger than PI")

        buffer += VertDispl(vi, alpha_vi)

      }
    }
    buffer.toSeq
  }
  /**
    * Computes the intersection between [a, b] and [orig, endpoint]
    */
  def intersectWithWindow(a: VertDispl, b: VertDispl, orig: VertDispl, endpoint: Option[VertDispl]): Option[VertDispl] = {
    val s1 = floating.FSegment(a, b)
    val res = endpoint match {
      // It's a segment
      case Some(ep) =>
        val s2 = floating.FSegment(orig, ep)
        intersectSegments(s1, s2)
      // It's a ray
      case None =>
        // The ray passing through orig
        val ray = floating.Ray2D(orig, orig.phi)
        intersectSegmentWithRay(s1, ray)
    }

    res.map(p => displacementInBetween(p, a, b))
  }

  /**
    * Compute angular displacement for a point s of a segment between v1 and v2.
    *
    * An important condition for termination of this function is that s.phi is between
    * v1.alpha and v2.alpha for some integer number of full 2Pi-periods.
    *
    */
  def displacementInBetween(s: PointPolar, v1: VertDispl, v2: VertDispl): VertDispl = {
    val bot = scala.math.min(v1.alpha, v2.alpha)
    val top = scala.math.max(v1.alpha, v2.alpha)
    if (bot =~= top) return VertDispl(s, bot)

    def locate(): VertDispl = {
      var tmp = s.phi
      while (tmp >~ top) {
        tmp = tmp - PI2
      }
      while (tmp <~ bot) {
        tmp = tmp + PI2
      }
      assert(bot <=~ tmp && tmp <=~ top, "Something is wrong here" +(s, v1, v2))
      VertDispl(s, tmp)
    }
    locate()
    //runWithTimer(locate, 2000, "displInBetween", (s, v1, v2))
  }

  /**
    * Returns the new stack and the last popped element
    */
  def locateSj(vi: VertDispl, vi1: VertDispl, sj1: VertDispl, ss: Seq[VertDispl]): (MStack[VertDispl], VertDispl) = {
    val (sj, stail) = (ss.head, ss.tail)
    if (sj.alpha <~ vi1.alpha &&
        vi1.alpha <=~ sj1.alpha) {
      return (new MStack[VertDispl]().pushAll(ss.reverse), sj1)
    }
    val y = intersectSegments(floating.FSegment(vi, vi1), floating.FSegment(sj, sj1))
    if (vi1.alpha <=~ sj.alpha && sj.alpha =~= sj1.alpha &&
        // intersection y is strictly between sj and sj1
        y.isDefined && y.get =!= sj && y.get =!= sj1) {
      return (new MStack[VertDispl]().pushAll(ss.reverse), sj1)
    }
    locateSj(vi, vi1, sj, stail)
  }

  /**
    * Representation of the polygon vertices depending on the position of the view point z
    */
  class VsRep(val vs: Seq[PointPolar], val zIsOrigin: Boolean) {
    // BUG: Initial paper has a bug, assuming that n should be
    // vs.size - 2 is z is origin (see Section 2, 1st paragraph)
    val n = if (zIsOrigin) vs.size - 1 else vs.size

    def apply(i: Int): VertDispl = displacements(i)

    private val displacements =
      if (zIsOrigin) angularDisplacementSeq(vs.tail)
      else angularDisplacementSeq(vs)
  }

}



