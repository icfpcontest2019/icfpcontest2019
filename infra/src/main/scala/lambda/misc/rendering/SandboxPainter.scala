package lambda.misc.rendering

import lambda.geometry._
import lambda.geometry.floating.examples.FPolygonExamples._
import lambda.geometry.floating.generators.PolygonCombinatorUtils._
import lambda.geometry.floating.generators.PolygonGenerators._
import lambda.geometry.floating.generators.RandomCrazyPolygonGenerator
import lambda.geometry.floating.triangles.DualGraphUtils
import lambda.geometry.floating.visibility.VisibilityChecker
import lambda.geometry.floating.{FPoint, FPolygon, FPointUtils}
import lambda.misc.artgallery.{GuardInputParser, GuardSolutionParser}
import org.scalacheck.Gen

import scala.io.Source

/**
  * Various one-time-used runners to generate polygons for different sorts
  *
  * @author Ilya Sergey
  */


import lambda.geometry.floating.generators.PolygonPropertyUtils._
import lambda.misc.rendering.ArtGalleryPainter._

object SandboxPainter {

  def main(args: Array[String]) {

    val freqs = Seq(10, 1, 2, 2, 2, 2)
    val freqs4 = Seq(4, 4, 2, 2, 2, 2)
    val freqs2 = Seq(10)

    val polygonsToAttach = Seq(
      generateNormalizedRectangle,
      prep(chvatal_comb1),
      prep(kittyPolygon),
      prep(triangle3),
      prep(triangle2),
      prep(generateConvexPolygon(12))
    )

    val polygonsToAttach2 = Seq(generateNormalizedRectangle)

    val convexPolygons = Seq(prepNoScale(generateConvexPolygon(20)))

    val polygonsToAttach3 = Seq(prep(triangle2))
    val freqs3 = Seq(8, 2)

    val posStrategy0 = (l: Double) => Some((0, l.toInt))
    val posStrategy1 = RandomCrazyPolygonGenerator.positionStrategy
    val posStrategy2 = (l: Double) => {
      if (l < 3) None
      else Some((1, 2))
    }

    val posStrategy3 = (l: Double) => {
      if (l.toInt < 3) None
      else {
        val startOffset = randomIntBetween(1, l.toInt - 1)
        val endOffset = randomIntBetween(startOffset + 1, l.toInt)
        Some((startOffset, endOffset))
      }
    }

    // totally random stuff
//        generateStuff(polygonsToAttach, freqs4, simple3Rectangle, posStrategy1, 80)

    // triangular stuff
//    generateStuff(polygonsToAttach3, freqs3, triangle1, posStrategy1, 150)

    // rectilinear stuff
        generateStuff(polygonsToAttach2, freqs2, flat3Polygon, posStrategy1, 5)

    // dumbells
//        generateStuff(convexPolygons, freqs3, big_rect, posStrategy1, 10)
  }

  def generateStuff(polygonsToAttach: Seq[LazyPolygon], freqs: Seq[Int],
                    base: FPolygon, posStrategy: Double => Option[(Int, Int)],
                    gens: Int): Unit = {
    val attachments: Gen[LazyPolygon] =
      Gen.frequency(freqs.zip(polygonsToAttach.map(p => Gen.const(p))): _*)
    val pc = generatePolygon(base, attachments, Gen.choose(2, 5), gens, posStrategy)
    println(pc.pol.vertices.size)
//    val td = DualGraphUtils.chavatalVisibilitySet(pc.pol)
    val td = Seq.empty
    drawShape(pc, td, drawCE = false)
    println(pc)
    println(FPolygon(td))
  }

}


object HardPainter {

  def main(args: Array[String]) {
    val pol = floating.FPolygon(Seq((1, 1), (1, -3), (4, -3), (4, -1), (3, -1), (3, -2), (2, -2), (2, 0), (6, 0),
      (6, -1), (7, -1), (7, 1)).
        map { case v => FPointUtils._point2D(v) })

        println(pol.vertices.size)

//            val td = DualGraphUtils.chavatalVisibilitySet(pol)
    // (5, 0), (0, 5), (-2, -2)

    val td = Seq((7, -1), (1, 1), (4, -1)).map(FPointUtils._point2D)

    drawShape(pol, td, drawCE = true, drawTriang = true)

  }

}


object AdaptCheckSolution {

  def main(args: Array[String]) {
    val pol = Seq().map(FPointUtils._point2D)

    val td = Seq().map(FPointUtils._point2D)
    val res = VisibilityChecker.checkVisibility(FPolygon(pol), td)
    println(res)
    println(pol.size)
    println()
    drawShape(FPolygon(pol), td)
    println(s"${FPolygon(pol)}; ${FPolygon(td.distinct)}")
  }
}

/**
  * Great example: almost invisible point, couldn't be spotted by an eye, yet invisible
  */
object JoshuaPainter {

  def main(args: Array[String]) {

    val pol = floating.FPolygon(Seq((3, 3), (1.5, 4.5), (1.5, 6), (3, 7.5), (2.2928932188134525, 8.207106781186548), (6.181980515339463, 12.803300858899107), (1.585786437626905, 8.914213562373096), (1.5, 9), (1.5, 10.5), (3, 12), (1.5, 13.5), (6.5, 13.5), (6.5, 14.5), (1.5, 14.5), (1.5, 15), (3, 16.5), (2.1055728090000843, 16.947213595499957), (2.5527864045000417, 17.841640786499873), (8.142956348249516, 15.605572809000087), (3, 18.73606797749979), (2.1055728090000843, 19.183281572999746), (1.2111456180001685, 17.394427190999913), (0, 18), (0, 0), (3, 0), (6, 1), (3, 1), (4, 1.5), (7, 1.5), (4.763932022500205, -4.090169943749471), (6.323772863187443, -1.527574276906157), (6.927782454342166, -1.895232288913381), (7.0459582439159165, -2.381066090494355), (6.678300231908692, -2.9850756816490778), (7.282309823063416, -3.352733693656302), (7.400485612637165, -3.8385674952372755), (7.0328276006299415, -4.442577086391999), (7.636837191784665, -4.810235098399223), (7.755012981358416, -5.296068899980195), (7.3873549693511915, -5.90007849113492), (7.10451225687657, -7.879977478457253), (4.983191913316928, -8.587084259643799), (-0.9565050486500715, -7.738556122219936), (-2.794982679735093, -6.465763916084146), (-3.0778253922097134, -8.44566290340648), (6.821669544401949, -9.859876465779585), (6.535885651054865, -11.86036371920916), (8.362140533745599, -6.123222174550508), (8.477198362086886, -6.149560713568394), (7.29544046634939, -1.2912226977586574), (7.697918365697627, -1.1933226681874651), (8.552116921312066, -1.7132696150832114), (8.886368530030758, -3.087415117593396), (10.260514032540941, -2.753163508874704), (11.114712588155381, -3.2731104557704476), (11.448964196874071, -4.647255958280635), (12.823109699384256, -4.313004349561942), (13.677308254998694, -4.832951296457686), (14.011559863717384, -6.207096798967874), (15.905652313123317, -5.018646634634738), (7.363666756978935, 0.18082283432271895), (7.894427190999915, 1.052786404500042), (8, 1), (14, 1.5), (11.757772043949016, 10.718048263765137), (11.010362725265361, 1.7491364395612203), (10.013816967020482, 1.83218191941496), (10.844271765557878, 11.797639501863758), (8.851180249068117, 11.963730461571236), (8.020725450530723, 1.9982728791224393), (8, 2), (3, 2)
    ).
        map { case v => FPointUtils._point2D(v) })

    println(pol.vertices.size)

//    val td = DualGraphUtils.chavatalVisibilitySet(pol)
//    println(Polygon(td))
//    println()
//    println(td)
    val td = Seq((10.57, 9.4588), (12.0199, 8.20667), (1.4655, 12.4584), (2.74686, 0.673664), (3.44488, -9.24531), (7.67462, -3.07733), (5.76697, 14.1303), (1.79445, 8.02645), (3.78012, 18.08), (7.07661, 1.62677), (9.61021, -2.59891), (7.34606, -6.48815), (14.7262, -4.93806), (1.65211, 15.489), (-2.7689, -7.0445), (11.5371, -4.18502), (2.4363, 17.7584), (7.13772, -4.29319), (3.27763, 1.33134)).map { case v => FPointUtils._point2D(v) }

    val res = VisibilityChecker.isInvisible(pol, td, FPoint(7.655472832078246, -1.1858709853435803))
    println(res)

    drawShape(pol, td)

  }

}

object PrecisionBug {

  def main(args: Array[String]) {

    val pol = floating.FPolygon(Seq((2.5, 7), (3, 7.5), (3.447213595499958, 8.394427190999917), (6.130495168499705, 7.052786404500042), (5.236067977499788, 5.26393202250021), (6.130495168499705, 4.816718427000252), (7.024922359499621, 6.605572809000083), (8.024922359499621, 3.605572809000083), (8.024922359499621, 6.605572809000083), (8.142956348249516, 6.605572809000083), (10.826237921249263, 5.263932022500209), (11.4970583144992, 4.369504831500292), (11.94427190999916, 5.263932022500211), (3.8944271909999157, 9.288854381999833), (5.23606797749979, 11.97213595499958), (4.341640786499873, 12.419349550499536), (3.670820393249936, 12.195742752749558), (3.2236067977499783, 12.419349550499536), (3, 13.090169943749473), (2.3291796067500625, 12.866563145999494), (1.8819660112501047, 13.090169943749473), (1.6583592135001255, 13.760990336999411), (0.9875388202501885, 13.537383539249431), (0.540325224750231, 13.76099033699941), (0.3167184270002519, 14.431810730249348), (-0.3832815729997484, 14.531810730249347), (-0.6832815729997488, 14.931810730249346), (-0.5832815729997494, 15.631810730249347), (-1.2832815729997498, 15.731810730249347), (-1.58328157299975, 16.131810730249345), (-1.4832815729997506, 16.831810730249344), (-2.1832815729997512, 16.931810730249346), (-2.483281572999752, 17.331810730249344), (-2.383281572999752, 18.031810730249344), (-3.483281572999752, 17.831810730249344), (-0.4832815729997475, 13.831810730249346), (-0.5777087639996639, 13.76099033699941), (3.894427190999915, 11.52492235949962), (2.1055728090000843, 7.947213595499958), (2, 8), (2, 7), (0.5, 5.5), (-1, 5.5), (-2.5, 7), (-4, 5.5), (-5.5, 5.5), (-7, 7), (-8.5, 5.5), (-10, 5.5), (-11.5, 7), (-13, 4), (2, 4), (2, 3), (0, 3), (0, 1), (-3, 1), (-3, 0), (3, 0), (3, 3), (2.5, 3.5), (2.5, 4), (3, 4.5), (2.5, 5), (2.5, 5.5), (3, 6), (2.5, 6.5)).
        map { case v => FPointUtils._point2D(v) })

    println(pol.vertices.size)

//    val td = DualGraphUtils.chavatalVisibilitySet(pol)
    (8.024922359499621, 3.605572809000083)
    val td = Seq((3, 7.5), (8.02492235949962, 3.60557280900008)).map { case v => FPointUtils._point2D(v) }
//    println(Polygon(td))
//    println()
    println(td.size)
//    val td = Seq((3, 2.5), (5, 0), (10, -2)).map { case v => PointUtils._point2D(v) }
//    val res = VisibilityChecker.isInvisible(pol, td, Point2D(15.546306389470876, 5.124597469583302))
//    println(res)

    drawShape(pol, td, drawCE = false)

  }

}

object ExamplePainter {

  def main(args: Array[String]) {

    val pol = floating.FPolygon(Seq((0, 0), (1, 0), (1, -1), (-4, -1), (-4, 0), (-1, 0), (-1,
        1), (-4, 1), (-4, 3), (-5, 3), (-5, 2), (-6, 2), (-6, 7), (0, 7), (0, 8)
      , (-2, 8), (-2, 11), (2, 11), (2, 12), (-2, 12), (-2, 13), (-3, 13), (-3,
          8), (-6, 8), (-6, 10), (-7, 10), (-7, 2), (-8, 2), (-8, 1), (-5, 1), (-5
          , -1), (-6, -1), (-6, -2), (1, -2), (1, -4), (-2, -4), (-2, -5), (-1, -5)
      , (-1, -6), (-6, -6), (-6, -7), (-1, -7), (-1, -8), (-5, -8), (-5, -9), (
          -3, -9), (-3, -10), (-8, -10), (-8, -11), (-3, -11), (-3, -12), (-2, -12)
      , (-2, -9), (-1, -9), (-1, -11), (0, -11), (0, -10), (3, -10), (3, -9), (
          0, -9), (0, -5), (1, -5), (1, -7), (2, -7), (2, -6), (4, -6), (4, -9), (5
          , -9), (5, -6), (6, -6), (6, -10), (7, -10), (7, -9), (9, -9), (9, -12),
      (4, -12), (4, -13), (9, -13), (9, -15), (1, -15), (1, -16), (2, -16), (2,
          -18), (-1, -18), (-1, -13), (-2, -13), (-2, -14), (-10, -14), (-10, -15)
      , (-9, -15), (-9, -20), (-16, -20), (-16, -21), (-9, -21), (-9, -22), (-8
          , -22), (-8, -15), (-7, -15), (-7, -18), (-6, -18), (-6, -15), (-2, -15),
      (-2, -18), (-5, -18), (-5, -19), (2, -19), (2, -20), (-3, -20), (-3, -21
          ), (2, -21), (2, -22), (-2, -22), (-2, -23), (2, -23), (2, -24), (3, -24)
      , (3, -16), (4, -16), (4, -22), (5, -22), (5, -16), (6, -16), (6, -19), (
          7, -19), (7, -16), (9, -16), (9, -17), (10, -17), (10, -9), (12, -9), (12
          , -8), (7, -8), (7, -6), (9, -6), (9, -5), (2, -5), (2, -4), (9, -4), (9,
          -3), (2, -3), (2, 0), (3, 0), (3, 1), (2, 1), (2, 2), (7, 2), (7, -2), (
          8, -2), (8, -1), (13, -1), (13, 0), (8, 0), (8, 2), (10, 2), (10, 3), (9,
          3), (9, 6), (13, 6), (13, 7), (9, 7), (9, 9), (8, 9), (8, 3), (2, 3), (2
          , 4), (1, 4), (1, 3), (0, 3), (0, 6), (-1, 6), (-1, 3), (-3, 3), (-3, 2),
      (1, 2), (1, 1), (0, 1)).map { case (a, b) => FPoint(a, b) })

    val pol2 = floating.FPolygon(Seq((0, 0), (3, 0), (3, 1), (2, 1), (2, 2), (7, 2), (7, -2), (8, -2),
      (8, -1), (13, -1), (13, 0), (8, 0), (8, 2), (10, 2), (10, 3), (2, 3), (2,
          4), (1, 4), (1, 3), (0, 3), (0, 6), (-1, 6), (-1, 3), (-3, 3), (-3, 2),
      (1, 2), (1, 1), (0, 1)).map { case (a, b) => FPoint(a, b) })

    drawShape(pol, pol.vertices)

    drawShape(pol2, pol2.vertices)

  }

}

/**
  * Produce model solution
  */
object ModelGuardsSolution {

  def main(args: Array[String]) {
    val path = args(0)
    val spath = args(1)
    val map = Source.fromFile(path).getLines.toSeq.map(GuardInputParser(_).get).toMap
    val sol = Source.fromFile(spath).getLines.toSeq.map(GuardSolutionParser(_).get)

    for ((pn, guards) <- sol) {
      val pol = map(pn)
      val res = VisibilityChecker.checkVisibility(pol, guards)._1
      assert(res)
      println(s"Tested model solution successfully for polygon ${pn}.")
    }

    //testExhausively(map)
  }

  def generateAndCheckSolution(map: Map[Int, FPolygon]): Unit = {
    for (k <- map.keySet.toSeq.sorted) {
      val pol: FPolygon = map(k)
      val guards = DualGraphUtils.chavatalVisibilitySet(pol)
      val sz = guards.size
      val pvs = pol.vertices.size
      val res = VisibilityChecker.checkVisibility(pol, guards)._1
      assert(res)
      assert(!(sz * 3 > pvs + 1))
      print(s"$k: [${sz}/${pvs}, ${!(sz * 3 > pvs + 1)}], visible: $res ")
      print(guards.mkString(", "))
      println()
    }
  }
  def testExhausively(map: Map[Int, FPolygon]): Unit = {
    for (k <- map.keySet.toSeq.sorted) {
      val pol: FPolygon = map(k)
      val guards = pol.vertices
      val res = VisibilityChecker.checkVisibility(pol, guards)._1
      assert(res)
      println(s"$k: tested, okay ")
    }

  }
}

object UclLogoPainter {

  def main(args: Array[String]) {


    val s1 = Seq(
    (100, 0), (100, 10), (90, 15), (90, 25), (-80, 25), (-80, 27),
    // collumns
    (-69, 27), (-69, 95), (-48, 95), (-48, 27),
    (-31, 27), (-31, 95), (-9, 95), (-9, 27),
    (9, 27), (9, 95), (31, 95), (31, 27),
    (48, 27), (48, 95), (69, 95), (69, 27),
     (85, 27), (85, 100),
    (88, 110), (90, 110), (92, 113), (50, 125), (50, 130), (48, 132),
    (48, 140), (45, 147), (43, 147),
    (37, 164), (33, 169), (23, 177), (13, 182),
    (13, 187), (10, 190), (10, 205), (13, 205), (2, 217),
    (3, 218), (3, 223),
    (-3, 223), (-3, 218),
    (-2, 217), (-13, 205), (-10, 205), (-10, 190), (-13, 187),
    (-13, 182), (-23, 177), (-33, 169), (-37, 164),
    (-43, 147), (-45, 147), (-48, 140),
    (-48, 132), (-50, 130), (-50, 125), (-92, 113), (-90, 110), (-88, 110),
    (-85, 100), (-85, 25), (-90, 25), (-90, 15), (-100, 10), (-100, 0))


    val pol = FPolygon(s1.map(FPointUtils._point2D))

    println(pol)
   // println()

//    val td = Seq(Point2D(0, 195))

    val td = DualGraphUtils.chavatalVisibilitySet(pol)

    //val res = VisibilityChecker.checkVisibility(pol, td)

    //println(res)
    println()
    //println(td)

    drawShape(pol, td, drawCE = false)

  }

}

object EnterprisePainter {

  def main(args: Array[String]) {
    val pol = FPolygon(Seq((152, 26), (151, 20), (274, 16), (280, 21), (368, 28), (366, 35), (256, 43), (316, 135), (375, 135), (386, 128), (406, 130), (412, 125), (407, 117), (432, 85), (437, 86), (500, 63), (559, 60), (629, 78), (691, 114), (690, 211), (629, 246), (559, 262), (500, 264), (437, 238), (433, 239), (408, 208), (413, 199), (407, 196), (386, 196), (376, 190), (316, 187), (256, 281), (366, 289), (368, 295), (280, 302), (274, 308), (151, 315), (152, 301), (59, 295), (59, 289), (232, 282), (250, 229), (140, 225), (140, 219), (196, 215), (237, 179), (186, 172), (181, 165), (181, 161), (186, 154), (237, 146), (196, 108), (140, 105), (140, 98), (250, 96), (232, 43), (59, 34), (59, 29)).map(FPointUtils._point2D))
    println(pol)
    println()
    val td = DualGraphUtils.chavatalVisibilitySet(pol)
    println()
    println(td)
    drawShape(pol, td)
  }
}

