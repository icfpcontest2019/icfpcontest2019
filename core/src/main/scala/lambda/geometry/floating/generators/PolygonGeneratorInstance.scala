package lambda.geometry.floating.generators

import lambda.geometry.floating.FPolygon
import org.scalacheck.{Arbitrary, Gen}


/**
  * @author Ilya Sergey
  */

abstract trait PolygonGeneratorInstance {

  def generate(numGenerations: Int): Gen[CompositePolygon]
  
  def generateFlat(numGenerations: Int): Gen[FPolygon] =
    for {composite <- generate(numGenerations)} yield composite.pol
  
  protected val numGen : Int

  implicit lazy val arbPolygonCombinator: Arbitrary[CompositePolygon] =
    Arbitrary(generate(numGen))

}





