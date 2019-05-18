package lambda.contest.generators

import lambda.contest.generators.Attachments._
import lambda.contest.generators.BasePolygons._
import lambda.geometry._
import lambda.geometry.floating.generators.PolygonGenerators.{AttachmentStrategy, prepNoScale}
import lambda.geometry.floating.generators.PolygonPropertyUtils._
import lambda.geometry.floating.{FPoint, FPolygon, RenderUtils}
import lambda.geometry.integer.IPolygon

import scala.util.Random

/**
  * @author Ilya Sergey
  */
object ContestGenerators {

  def boundingBoxSize(z: Int) = (p: FPolygon) => {
    val (FPoint(xa, ya), FPoint(xb, yb)) = RenderUtils.getPolygonBoundingBox(p)
    val dx = xb - xa
    val dy = yb - ya
    dx + 1 <= z && dy + 1 <= z
  }

  def isWithinBoxAtOrigin(boundPoly: Option[IPolygon] = None) = (p: FPolygon) =>
    boundPoly match {
      case Some(box) =>
        val boxAtOrigin = box.shiftToOrigin.toFPolygon
        val (z, _) = RenderUtils.getPolygonBoundingBox(p)
        val q = p.shiftToOrigin(z)
        boxAtOrigin.contains(q)
      case None => true
    }


  def isWithinBoundary(boundOpt: Option[FPolygon] = None) = (p: FPolygon) =>
    boundOpt match {
      case Some(x) => x.contains(p) &&
        !x.edgeIntersect(p)
      case None => true
    }

  /** ****************************************************************************/
  /*                           Polygon Generators                               */
  /** ****************************************************************************/

  /*----------------------------------------------------------------------------*/
  /*                                Obstacles                                   */
  /*----------------------------------------------------------------------------*/

  private val tetrisAtts = List(tetrisStick, tetrisL, tetrisR, tetris2, tetris3l, tetrisR)
  private val baseTetrisPolygons = tetrisAtts.map(_.toIPolygon.shiftToOrigin)
  private val baseTetrisPolygonsRotated =
    baseTetrisPolygons.map(p => IPolygon(p.vertices.map(_.rotateLeft)).shiftToOrigin)

  val simpleStrategy: AttachmentStrategy = {
    case (l, rect) =>
      if (l < 3) None else {
        val startOffset = randomIntBetween(1, l.toInt - 2)
        val endOffset = startOffset + 1
        Some((startOffset, endOffset, 1))
      }
  }

  val simpleStrategy2: AttachmentStrategy = {
    case (l, rect) =>
      if (l < 5) None else {
        val startOffset = randomIntBetween(1, l.toInt - 2)
        val endOffset = startOffset + 1
        Some((startOffset, endOffset, 1))
      }
  }

  // For obstacles within 5 x 5 bounding box
  def obstacles_5x5(boundOpt: Option[IPolygon] = None): ContestPolygonGenerator = {
    val bases = (baseTetrisPolygons ++ baseTetrisPolygonsRotated ++ List(square4.toIPolygon)).map(_.toFPolygon)
    val baseFreqs = bases.map(_ => 1).tail ++ List(10)
    val cond: FPolygon => Boolean = p => boundingBoxSize(5)(p) && isWithinBoxAtOrigin(boundOpt)(p)
    val atts = List(scalableRectangles)

    ContestPolygonGenerator(bases, baseFreqs, atts, List(1), cond, (1, 1))
  }

  // For obstacles within 10x10 box
  def obstacles_10x10(boundOpt: Option[IPolygon] = None): ContestPolygonGenerator = {
    val tetrisBases = (baseTetrisPolygons ++ baseTetrisPolygonsRotated).map(_.toFPolygon)
    val otherBases = List(square4, square5, wPoly)
    val bases = tetrisBases ++ otherBases
    val baseFreqs = tetrisBases.map(_ => 1) ++ List(10, 8, 3)
    val atts = List(scalableRectangles, scalableSquares)
    val attFreqs = List(3, 3)
    val cond: FPolygon => Boolean = p => boundingBoxSize(10)(p) && isWithinBoxAtOrigin(boundOpt)(p)
    ContestPolygonGenerator(bases, baseFreqs, atts, attFreqs, cond, (1, 5))
  }

  // For obstacles within 20x20 box
  def obstacles_20x20(boundOpt: Option[IPolygon] = None): ContestPolygonGenerator = {
    val tetrisBases = (baseTetrisPolygons ++ baseTetrisPolygonsRotated).map(_.toFPolygon)
    val otherBases = List(square6, square6, square5, square4, wPoly)
    val bases = tetrisBases ++ otherBases
    val baseFreqs = tetrisBases.map(_ => 1) ++ List(10, 10, 8, 6, 3)
    val atts = List(scalableRectangles, scalableSquares) ++
      tetrisAtts.map(a => (prepNoScale(a), simpleStrategy))
    val attFreqs = List(5, 5) ++ tetrisAtts.map(_ => 1)
    val cond: FPolygon => Boolean = p => boundingBoxSize(20)(p) && isWithinBoxAtOrigin(boundOpt)(p)
    ContestPolygonGenerator(bases, baseFreqs, atts, attFreqs, cond, (2, 7))
  }

  // For obstacles within 30x30 box
  def obstacles_30x30(boundOpt: Option[IPolygon] = None): ContestPolygonGenerator = {
    val tetrisBases = (baseTetrisPolygons ++ baseTetrisPolygonsRotated).map(_.toFPolygon)
    val otherBases = List(square4, square5, wPoly)
    val bases = tetrisBases ++ otherBases
    val baseFreqs = tetrisBases.map(_ => 1) ++ List(3, 1, 3)
    val atts = List((generateNormalizedRectangle, simpleStrategy2)) ++
      List(scalableRectangles, scalableSquares) ++
      tetrisAtts.map(a => (prepNoScale(a), simpleStrategy))
    val attFreqs = List(5) ++ List(10, 10) ++ tetrisAtts.map(_ => 1)
    val cond: FPolygon => Boolean = p => boundingBoxSize(30)(p) && isWithinBoxAtOrigin(boundOpt)(p)
    ContestPolygonGenerator(bases, baseFreqs, atts, attFreqs, cond, (2, 8))
  }

  // For obstacles within 50x50 box
  def obstacles_50x50(boundOpt: Option[IPolygon] = None): ContestPolygonGenerator =
    largeRoomGenerator(50, boundOpt)

  /*----------------------------------------------------------------------------*/
  /*                                  Large rooms                               */
  /*----------------------------------------------------------------------------*/

  def largeRoomGenerator(size: Int, boundOpt: Option[IPolygon] = None) = {
    import Attachments._
    import BasePolygons._

    val basePolygons = simpleBases
    val baseFreqs = List(10, 12, 4)

    ///////////////////////////////////////////////////

    val attachments = List(
      scalableRectangles,
      scalableSquares,

      scalableRectangles3,
      scalableSquares3,
      scalableFlatRectangles,

      scalablePyramide,

      scalableSquares2,
      scalableRectangles2,
      scalableSquares4,
      scalableRectangles4
    )

    val fs1 = Random.nextInt(20) + 15
    val fs2 = Random.nextInt(20) + 15

    val fs3 = Random.nextInt(10) + 2
    val fs4 = Random.nextInt(10) + 2
    val fs5 = Random.nextInt(10) + 5

    val fs6 = Random.nextInt(5) + 5

    val fs7 = Random.nextInt(10) + 0
    val fs8 = Random.nextInt(10) + 0
    val fs9 = Random.nextInt(10) + 0
    val fs10 = Random.nextInt(10) + 0

    val attFqs = List(fs1, fs2, fs3, fs4, fs5, fs6, fs7, fs8, fs9, fs10)

    ///////////////////////////////////////////////////
    val tetrisAtts = List(
      tetrisStickScale, // tetrisStickNoScale,
      tetrisLScale, //tetrisLNoScale,
      tetrisRScale, //tetrisRNoScale,
      tetris3lScale, //tetris3lNoScale,
      tetris3rScale, //tetris3rNoScale,
      tetris2Scale //tetris2NoScale,
    )
    val tetrisFreqs = List(2)

    val includeLollis = size >= 200

    ///////////////////////////////////////////////////
    val (lols, lfrews) = if (includeLollis) {
      (List(unscalableLollis), List(5))
    } else (Nil, Nil)


    ///////////////////////////////////////////////////

    val atts = attachments ++ lols ++ tetrisAtts
    val attFreqs = attFqs ++
      lfrews ++
      tetrisFreqs ++ tetrisFreqs ++ tetrisFreqs ++
      tetrisFreqs ++ tetrisFreqs ++ tetrisFreqs


    val stickSizes = (3, 6)

    def cond: FPolygon => Boolean = p =>
      boundingBoxSize(size)(p) &&
        isWithinBoxAtOrigin(boundOpt)(p)

    ContestPolygonGenerator(basePolygons, baseFreqs,
      atts, attFreqs, cond, stickSizes)
  }


}


import lambda.geometry.floating.FPolygonUtils._

object BasePolygons {
  /* ------------------------------------------------------------- */
  /*                          Base polygons                        */
  /* ------------------------------------------------------------- */

  val square4: FPolygon =
    Seq((0, 0), (4, 0), (4, 4), (0, 4))
  val square5: FPolygon =
    Seq((0, 0), (5, 0), (5, 5), (0, 5))
  val square6: FPolygon =
    Seq((0, 0), (6, 0), (6, 6), (0, 6))
  val square10: FPolygon =
    Seq((0, 0), (10, 0), (10, 10), (0, 10))
  //  val cross2: FPolygon =
  //    Seq((0, 0), (1, 0), (1, -2), (3, -2), (3, 0), (4, 0), (4, 1), (6, 1), (6, 3), (4, 3),
  //      (4, 4), (3, 4), (3, 6), (1, 6), (1, 4), (0, 4), (0, 3), (-2, 3), (-2, 1), (0, 1))
  val wPoly: FPolygon =
  Seq((0, 0), (5, 0), (5, 3), (4, 3), (4, 1), (1, 1), (1, 3), (0, 3))


  /* ------------------------------------------------------------- */
  /*                          Combinations                        */
  /* ------------------------------------------------------------- */

  val simpleBases = List(square6, square10, wPoly)

}

object Attachments {

  /* ------------------------------------------------------------- */
  /*         Random polygons with strategies                       */
  /* ------------------------------------------------------------- */
  val square = prepNoScale(
    FPolygon(Seq(
      FPoint(0, 0), FPoint(0, 1),
      FPoint(-1, 1), FPoint(-1, 0))))

  val flatRect = prepNoScale(
    FPolygon(Seq(
      FPoint(0, 0), FPoint(0, 0.5),
      FPoint(-1, 0.5), FPoint(-1, 0))))

  val scalableRectangles = (generateNormalizedRectangle, unitScalingStrategy)
  val scalableRectangles2 = (generateNormalizedRectangle, unit3ScalingStrategy)
  val scalableRectangles4 = (generateNormalizedRectangle, unit4ScalingStrategy)
  val scalableRectangles3 = (generateNormalizedRectangle, unitAttachmentStrategy3)
  val scalableFlatRectangles = (generateNormalizedRectangle, flatScalingStrategy)

  val scalableSquares = (square, unitScalingStrategy)
  val scalableSquares2 = (square, unit3ScalingStrategy)
  val scalableSquares4 = (square, unit4ScalingStrategy)
  val scalableSquares3 = (square, unitAttachmentStrategy3)

  val unscalableLollis = (prepNoScale(lolli), noScalingStrategy)

  val scalablePyramide = (prepNoScale(pyramide), unit3ScalingStrategy)

  val tetrisStickScale = (prepNoScale(tetrisStick), unit3ScalingStrategy)
  val tetrisStickNoScale = (prepNoScale(tetrisStick), noScalingStrategy)
  val tetrisStickUnit = (prepNoScale(tetrisStick), unitAttachmentStrategy)
  val tetrisStickUnit3 = (prepNoScale(tetrisStick), unitAttachmentStrategy3)

  val tetrisLScale = (prepNoScale(tetrisL), unit3ScalingStrategy)
  val tetrisLNoScale = (prepNoScale(tetrisL), noScalingStrategy)
  val tetrisLUnit = (prepNoScale(tetrisL), unitAttachmentStrategy)
  val tetrisLUnit3 = (prepNoScale(tetrisL), unitAttachmentStrategy3)

  val tetrisRScale = (prepNoScale(tetrisR), unit3ScalingStrategy)
  val tetrisRNoScale = (prepNoScale(tetrisR), noScalingStrategy)
  val tetrisRUnit = (prepNoScale(tetrisR), unitAttachmentStrategy)
  val tetrisRUnit3 = (prepNoScale(tetrisR), unitAttachmentStrategy3)

  val tetris2Scale = (prepNoScale(tetris2), unit3ScalingStrategy)
  val tetris2NoScale = (prepNoScale(tetris2), noScalingStrategy)
  val tetris2Unit = (prepNoScale(tetris2), unitAttachmentStrategy)
  val tetris2Unit3 = (prepNoScale(tetris2), unitAttachmentStrategy3)

  val tetris3lScale = (prepNoScale(tetris3l), unit3ScalingStrategy)
  val tetris3lNoScale = (prepNoScale(tetris3l), noScalingStrategy)
  val tetris3lUnit = (prepNoScale(tetris3l), unitAttachmentStrategy)
  val tetris3lUnit3 = (prepNoScale(tetris3l), unitAttachmentStrategy3)

  val tetris3rScale = (prepNoScale(tetris3r), unit3ScalingStrategy)
  val tetris3rNoScale = (prepNoScale(tetris3r), noScalingStrategy)
  val tetris3rUnit = (prepNoScale(tetris3r), unitAttachmentStrategy)
  val tetris3rUnit3 = (prepNoScale(tetris3r), unitAttachmentStrategy3)

  /* ------------------------------------------------------------- */
  /*                      Polygons to attach                       */
  /* ------------------------------------------------------------- */

  lazy val lolli: FPolygon = Seq((0, 0), (0, 10),
    (10, 10), (10, 30), (-10, 30), (-10, 10),
    (-1, 10), (-1, 0))

  // Tetris attachments

  lazy val tetrisStick: FPolygon = Seq((0, 0), (0, 4), (-1, 4), (-1, 0))

  lazy val tetrisL: FPolygon = Seq((0, 0), (0, 3), (-2, 3), (-2, 2), (-1, 2), (-1, 0))

  lazy val tetrisR: FPolygon = Seq((0, 0), (0, 2), (1, 2), (1, 3), (-1, 3), (-1, 0))

  lazy val tetris2: FPolygon = Seq((0, 0), (0, 2), (-1, 2), (-1, 3), (-2, 3), (-2, 1), (-1, 1), (-1, 0))

  lazy val tetris3l: FPolygon = Seq((0, 0), (0, 3), (-1, 3), (-1, 2), (-2, 2), (-2, 1), (-1, 1), (-1, 0))

  lazy val tetris3r: FPolygon = Seq((0, 0), (0, 1), (1, 1), (1, 2), (0, 2), (0, 3), (-1, 3), (-1, 0))


  lazy val pyramide: FPolygon = Seq((0, 0),
    (0, 10), (-8, 10),
    (-8, 8), (-6, 8),
    (-6, 6), (-4, 6),
    (-4, 4), (-2, 4),
    (-2, 2), (-1, 2),
    (-1, 0))


  /* ------------------------------------------------------------- */
  /*                        Strategies                             */
  /* ------------------------------------------------------------- */

  lazy val unitAttachmentStrategy: AttachmentStrategy = {
    case (l, rect) =>
      if (l != 1) None
      else Some((0, 1, 1))
  }

  lazy val unitAttachmentStrategy3: AttachmentStrategy = {
    case (l, rect) =>
      if (l != 1) None
      else Some((0, 1, 3))
  }

  lazy val noScalingStrategy: AttachmentStrategy = {
    case (l, rect) =>
      if (l < 3) None
      else {
        val startOffset = randomIntBetween(1, l.toInt - 2)
        val endOffset = startOffset + 1
        Some((startOffset, endOffset, 1))
      }
  }
  lazy val unitScalingStrategy: AttachmentStrategy = {
    case (l, rect) =>
      if (l < 3) None
      else {
        val startOffset = randomIntBetween(1, l.toInt - 2)
        val endOffset = randomIntBetween(startOffset + 1, l.toInt - 1)
        val scalingK = endOffset - startOffset
        Some((startOffset, endOffset, scalingK))
      }
  }

  lazy val unit3ScalingStrategy: AttachmentStrategy = {
    case (l, rect) =>
      if (l < 5) None
      else {
        val startOffset = randomIntBetween(1, l.toInt - 4)
        val endOffset = randomIntBetween(startOffset + 3, l.toInt - 1)
        val scalingK = endOffset - startOffset
        Some((startOffset, endOffset, scalingK))
      }
  }

  lazy val unit4ScalingStrategy: AttachmentStrategy = {
    case (l, rect) =>
      if (l < 56) None
      else {
        val startOffset = randomIntBetween(1, l.toInt - 5)
        val endOffset = randomIntBetween(startOffset + 4, l.toInt - 1)
        val scalingK = endOffset - startOffset
        Some((startOffset, endOffset, scalingK))
      }
  }

  lazy val flatScalingStrategy: AttachmentStrategy = {
    case (l, rect) =>
      if (l < 4) None
      else {
        val startOffset = randomIntBetween(1, l.toInt - 3)
        val endOffset = randomIntBetween(startOffset + 2, l.toInt - 1)
        val scalingK = (endOffset - startOffset) * 2
        Some((startOffset, endOffset, scalingK))
      }
  }


}
