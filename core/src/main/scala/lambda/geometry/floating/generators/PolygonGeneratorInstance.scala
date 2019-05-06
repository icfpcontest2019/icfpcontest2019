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





