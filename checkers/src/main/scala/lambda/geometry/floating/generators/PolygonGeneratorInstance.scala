package lambda.geometry.floating.generators

import lambda.geometry._
import lambda.geometry.floating.FPolygon
import lambda.geometry.floating.examples.FPolygonExamples._
import lambda.geometry.floating.generators.PolygonGenerators._
import lambda.geometry.floating.generators.PolygonPropertyUtils._
import org.scalacheck.{Arbitrary, Gen}


/**
  * @author Ilya Sergey
  */

abstract class PolygonGeneratorInstance {

  val basePolygons: Seq[FPolygon]
  val freqsBase: Seq[Int]
  val polygonsToAttach: Seq[LazyPolygon]
  val freqsAtt: Seq[Int]
  val attachedYSize: (Int, Int)
  val generations: Int
  // Positioning might fail if the edge is not suitable
  val positionStrategy: Double => Option[(Int, Int)]

  def generate(): Gen[CompositePolygon] =
    for {
      // choose base polygon
      base <- polyFreqs(freqsBase, basePolygons)
      // edge size
      eSize = Gen.choose(attachedYSize._1, attachedYSize._2)
      // number of extensions (generations)
      gSize <- Gen.choose(1, generations)
      // generate polygon
      attachments = polyFreqs(freqsAtt, polygonsToAttach)
    } yield generatePolygon(base, attachments, eSize, gSize, positionStrategy)

  def generateFlat(): Gen[FPolygon] =
    for {composite <- generate()} yield composite.pol

  implicit lazy val arbPolygonCombinator: Arbitrary[CompositePolygon] =
    Arbitrary(generate())

}

// TODO: emplain these generators and higher-order strategies
object RandomCrazyPolygonGenerator extends PolygonGeneratorInstance {

  val basePolygons = Seq(simple3Rectangle, flat3Polygon, weirdRectPolygon,
    chvatal_comb, simpleStarPolygon, lshaped3, sand5)

  val freqsBase = Seq(20, 20, 3, 2, 2, 1, 1)


  val polygonsToAttach = Seq(
    generateNormalizedRectangle,
    prep(chvatal_comb),
    prep(kittyPolygon),
    prep(triangle3),
    prep(triangle2)
  )

  val freqsAtt = Seq(10, 2, 5, 2, 1)
  val attachedYSize = (3, 3)
  val generations = 40

  /*
    val positionStrategy = (l: Double) => {
      val startOffset = randomIntBetween(0, l.toInt - 1)
      val endOffset = randomIntBetween(startOffset + 1, l.toInt - 1)
      Some((startOffset, endOffset))
    }
  */
  // better positions strategy
  val positionStrategy = (l: Double) => {
    if (l < 3) None
    else {
      val startOffset = randomIntBetween(1, l.toInt - 2)
      val endOffset = randomIntBetween(startOffset + 1, l.toInt - 1)
      Some((startOffset, endOffset))
    }
  }
}

object RandomRectilinearPolygonGenerator extends PolygonGeneratorInstance {
  val basePolygons = Seq(simple3Rectangle)
  val freqsBase = Seq(1)
  val polygonsToAttach = Seq(generateNormalizedRectangle)
  val freqsAtt = Seq(1)
  val attachedYSize = (2, 8)
  val generations = 60

  val positionStrategy = (l: Double) => {
    if (l < 3) None
    else {
      val startOffset = randomIntBetween(1, l.toInt - 2)
      val endOffset = randomIntBetween(startOffset + 1, l.toInt - 1)
      Some((startOffset, endOffset))
    }
  }
}

object RandomEasyPolygonGenerator extends PolygonGeneratorInstance {
  val basePolygons = Seq(triangle1, lShapedPolygon)
  val freqsBase = Seq(1, 4)
  val polygonsToAttach = basePolygons.map(prep(_))
  val freqsAtt = Seq(4, 1)
  val attachedYSize = (2, 6)
  val generations = 4

  val positionStrategy = RandomRectilinearPolygonGenerator.positionStrategy
}

