package lambda.geometry.floating.generators.old

import lambda.geometry.floating.examples.FPolygonExamples.{lShapedPolygon, triangle1}
import lambda.geometry.floating.generators.PolygonGenerators.prep
import lambda.geometry.floating.generators.PolygonGeneratorInstance
import lambda.geometry.floating.generators.rectilinear.ContestRectilinearPolygonGenerator

/**
  * @author Ilya Sergey
  */
object RandomEasyPolygonGenerator extends PolygonGeneratorInstance {
  val basePolygons = Seq(triangle1, lShapedPolygon)
  val freqsBase = Seq(1, 4)
  val polygonsToAttach = basePolygons.map(prep(_))
  val freqsAtt = Seq(4, 1)
  val attachedYSize = (2, 6)
  val generations = 4

  val positionStrategy = ContestRectilinearPolygonGenerator.positionStrategy
}
