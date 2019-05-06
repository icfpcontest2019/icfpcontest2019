package lambda.geometry.floating.generators.rectilinear

import lambda.geometry.floating.examples.FPolygonExamples
import lambda.geometry.floating.examples.FPolygonExamples._
import lambda.geometry.floating.generators.PolygonGenerators.generatePolygon
import lambda.geometry.floating.generators.PolygonPropertyUtils.{generate3Rectangle, generateNormalizedRectangle, generateNormalizedSquare, polyFreqs}
import lambda.geometry.floating.generators.{CompositePolygon, PolygonGeneratorInstance, PolygonGenerators, PolygonPropertyUtils}
import lambda.geometry.randomIntBetween
import org.scalacheck.Gen

/**
  * @author Ilya Sergey
  */
object ContestRectilinearPolygonGenerator extends PolygonGeneratorInstance {
  
  
  // TODO: Constrain the maximal size of the attached polygon
  // And the minimal size, too
  
  // TODO: Constrain the bounding box
  
  // TODO: Automate saving

  override def generate(): Gen[CompositePolygon] = for {
    // choose base polygon
    base <- polyFreqs(freqsBase, basePolygons)
    // edge size
    eSize = Gen.choose(attachedYSize._1, attachedYSize._2)
    // number of extensions (generations)
    gSize <- Gen.choose(1, generations)
    // generate polygon
    attachments = polyFreqs(freqsAtt, polygonsToAttach)
    cp = generatePolygon(base, attachments, eSize, gSize, positionStrategy)
    // TODO Adapt for better shape
  } yield cp
  
  val basePolygons = Seq(simple3Rectangle)
  val freqsBase = Seq(1)
  val polygonsToAttach = Seq(generateNormalizedRectangle, generateNormalizedSquare)
  val freqsAtt = Seq(3, 5)
  val attachedYSize = (2, 20)
  val generations = 100
  val positionStrategy = (l: Double) => {
    if (l < 4) None
    else {
      val startOffset = randomIntBetween(1, l.toInt - 3)
      val endOffset = randomIntBetween(startOffset + 2, math.min(startOffset + 6, l.toInt - 2))
      Some((startOffset, endOffset))
    }
  }
}
