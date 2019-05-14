package lambda.contest.generators

import lambda.geometry._
import lambda.geometry.floating.generators.PolygonGenerators.{AttachmentStrategy, prepNoScale}
import lambda.geometry.floating.generators.PolygonPropertyUtils._
import lambda.geometry.floating.{FPoint, FPolygon, RenderUtils}

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

  def isWithinBoundary(boundOpt: Option[FPolygon] = None) = (p: FPolygon) =>
    boundOpt match {
      case Some(x) => x.contains(p) && 
        !x.edgeIntersect(p)
      case None => true
    }

  def roomGenerator(size: Int, includeLollis: Boolean = false,
                    boundOpt: Option[FPolygon] = None) = {
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
        isWithinBoundary(boundOpt)(p)


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

  val simpleBases = List(square4, square10, wPoly)

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
      Some((0, 1, 1))
  }

  lazy val unitAttachmentStrategy3: AttachmentStrategy = {
    case (l, rect) =>
      if (l != 1) None
      Some((0, 1, 3))
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