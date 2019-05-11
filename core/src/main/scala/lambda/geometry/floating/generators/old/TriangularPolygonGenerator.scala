package lambda.geometry.floating.generators.old

import lambda.geometry.floating.FPolygon
import lambda.geometry.floating.examples.FPolygonExamples.triangle1
import lambda.geometry.floating.generators.PolygonGenerator
import org.scalacheck.Gen

/**
  * @author Ilya Sergey
  */
object TriangularPolygonGenerator extends PolygonGenerator {
  override val myOutFileName: String = "triangular.pol"
  override val myPolygonsToAttach = polygonsToAttach3
  override val myBasePolygon: FPolygon = triangle1
  override val myFreqs: Seq[Int] = freqs3
  override val myPolygonSize: Gen[Int] = Gen.choose(2, 6)
  override val myPositionStrategy = posStrategy1
  override val myGenerations: Int = 80
}
