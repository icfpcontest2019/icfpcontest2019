package lambda.geometry.floating.generators.old

import lambda.geometry.floating.examples.FPolygonExamples._
import lambda.geometry.floating.generators.PolygonGenerators.{AttachmentStrategy, generatePolygon, prep}
import lambda.geometry.floating.generators.PolygonPropertyUtils.{generateNormalizedRectangle, polyFreqs}
import lambda.geometry.floating.generators.{CompositePolygon, PolygonGeneratorInstance}
import lambda.geometry.randomIntBetween
import org.scalacheck.Gen

/**
  * @author Ilya Sergey
  */
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
  val positionStrategy: AttachmentStrategy = (l: Double, _) => {
    if (l < 3) None
    else {
      val startOffset = randomIntBetween(1, l.toInt - 2)
      val endOffset = randomIntBetween(startOffset + 1, l.toInt - 1)
      Some((startOffset, endOffset, endOffset - startOffset))
    }
  }

  override def generate(numGenerations: Int): Gen[CompositePolygon] =
    for {
      // choose base polygon
      base <- polyFreqs(freqsBase, basePolygons)
      // edge size
      eSize = Gen.choose(attachedYSize._1, attachedYSize._2)
      // number of extensions (generations)
      gSize <- Gen.choose(1, generations)
      // generate polygon
      attachments = polyFreqs(freqsAtt, polygonsToAttach.map (p => (p, positionStrategy)))
    } yield generatePolygon(base, attachments, _ => true, eSize, gSize)

  override protected val numGen = 10
}
