package lambda.contest.generators

import lambda.contest.generators.Attachments._
import lambda.contest.generators.BasePolygons._
import lambda.contest.generators.geodata.RenderEnterprise
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
  
  private val enter = List((40,0),(41,0),(41,2),(42,2),(42,3),(48,3),(48,4),(54,4),(54,5),(58,5),(58,9),(55,9),(55,10),(48,10),(48,11),(42,11),(42,12),(38,12),(38,15),(39,15),(39,18),(40,18),(40,22),(41,22),(41,25),(42,25),(42,28),(43,28),(43,31),(44,31),(44,35),(45,35),(45,38),(46,38),(46,41),(47,41),(47,44),(48,44),(48,47),(59,47),(59,46),(60,46),(60,45),(65,45),(65,41),(64,41),(64,40),(65,40),(65,38),(66,38),(66,35),(67,35),(67,32),(68,32),(68,29),(69,29),(69,28),(71,28),(71,27),(72,27),(72,26),(73,26),(73,25),(75,25),(75,24),(76,24),(76,23),(77,23),(77,22),(79,22),(79,21),(80,21),(80,20),(81,20),(81,19),(87,19),(87,18),(95,18),(95,19),(97,19),(97,20),(99,20),(99,21),(101,21),(101,22),(102,22),(102,23),(104,23),(104,24),(106,24),(106,25),(107,25),(107,26),(108,26),(108,27),(109,27),(109,28),(110,28),(110,29),(111,29),(111,31),(112,31),(112,32),(113,32),(113,33),(114,33),(114,34),(115,34),(115,35),(116,35),(116,37),(117,37),(117,38),(118,38),(118,39),(119,39),(119,40),(118,40),(118,79),(117,79),(117,81),(116,81),(116,82),(115,82),(115,83),(114,83),(114,84),(113,84),(113,86),(112,86),(112,87),(111,87),(111,88),(110,88),(110,89),(109,89),(109,90),(108,90),(108,92),(106,92),(106,93),(104,93),(104,94),(102,94),(102,95),(100,95),(100,96),(98,96),(98,97),(96,97),(96,98),(94,98),(94,99),(81,99),(81,98),(80,98),(80,97),(79,97),(79,96),(77,96),(77,95),(76,95),(76,94),(75,94),(75,93),(74,93),(74,92),(73,92),(73,91),(72,91),(72,90),(71,90),(71,89),(70,89),(70,90),(69,90),(69,87),(68,87),(68,85),(67,85),(67,82),(66,82),(66,79),(65,79),(65,73),(61,73),(61,72),(60,72),(60,71),(59,71),(59,70),(52,70),(52,69),(48,69),(48,72),(47,72),(47,76),(46,76),(46,79),(45,79),(45,82),(44,82),(44,85),(43,85),(43,89),(42,89),(42,92),(41,92),(41,95),(40,95),(40,99),(39,99),(39,102),(38,102),(38,105),(40,105),(40,106),(47,106),(47,107),(53,107),(53,108),(58,108),(58,112),(53,112),(53,113),(47,113),(47,114),(42,114),(42,115),(41,115),(41,117),(34,117),(34,118),(26,118),(26,119),(17,119),(17,114),(13,114),(13,113),(6,113),(6,112),(0,112),(0,108),(8,108),(8,107),(20,107),(20,106),(32,106),(32,101),(33,101),(33,95),(34,95),(34,89),(35,89),(35,86),(34,86),(34,85),(21,85),(21,84),(15,84),(15,80),(22,80),(22,79),(25,79),(25,78),(26,78),(26,76),(27,76),(27,75),(28,75),(28,73),(29,73),(29,71),(30,71),(30,69),(31,69),(31,67),(32,67),(32,65),(28,65),(28,64),(25,64),(25,63),(23,63),(23,61),(22,61),(22,57),(23,57),(23,55),(25,55),(25,54),(28,54),(28,53),(31,53),(31,52),(32,52),(32,50),(31,50),(31,48),(30,48),(30,46),(29,46),(29,45),(28,45),(28,43),(27,43),(27,41),(26,41),(26,39),(25,39),(25,38),(22,38),(22,37),(15,37),(15,33),(24,33),(24,32),(35,32),(35,29),(34,29),(34,23),(33,23),(33,17),(32,17),(32,12),(26,12),(26,11),(17,11),(17,10),(8,10),(8,9),(0,9),(0,6),(1,6),(1,5),(16,5),(16,4),(17,4),(17,2),(25,2),(25,1),(40,1))
    .map{case (x, y) => FPoint(x, y)}


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
