package lambda.geometry.floating.generators.rectilinear

import lambda.geometry.floating.examples.FPolygonExamples.simple3Rectangle
import lambda.geometry.floating.generators.PolygonGenerators.generatePolygon
import lambda.geometry.floating.generators.PolygonPropertyUtils.{generate3Rectangle, generateNormalizedRectangle, polyFreqs}
import lambda.geometry.floating.generators.{CompositePolygon, PolygonGeneratorInstance}
import lambda.geometry.randomIntBetween
import org.scalacheck.Gen

/**
  * @author Ilya Sergey
  */
object ContestRectilinearPolygonGenerator extends PolygonGeneratorInstance {
  
  
  // TODO: Constrain the maximal size of the attached polygon
  // And the minimal size, too

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
  val polygonsToAttach = Seq(generate3Rectangle, generateNormalizedRectangle)
  val freqsAtt = Seq(2, 7)
  val attachedYSize = (2, 10)
  val generations = 200
  val positionStrategy = (l: Double) => {
    if (l < 3) None
    else {
      val startOffset = randomIntBetween(1, l.toInt - 2)
      val endOffset = randomIntBetween(startOffset + 1, l.toInt - 1)
      Some((startOffset, endOffset))
    }
  }
}
