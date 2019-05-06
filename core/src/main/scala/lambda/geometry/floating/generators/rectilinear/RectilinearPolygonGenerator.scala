package lambda.geometry.floating.generators.rectilinear

import lambda.geometry.floating.FPolygon
import lambda.geometry.floating.examples.FPolygonExamples.simple3Rectangle
import lambda.geometry.floating.generators.PolygonGenerator
import org.scalacheck.Gen

/**
  * @author Ilya Sergey
  */
object RectilinearPolygonGenerator extends PolygonGenerator {
  override val myOutFileName: String = "rectilinear.pol"
  override val myBasePolygon: FPolygon = simple3Rectangle
  override val myFreqs: Seq[Int] = freqs2
  override val myPolygonSize: Gen[Int] = Gen.choose(1, 24)
  override val myPositionStrategy = posStrategy1
  override val myGenerations: Int = 15
  override val myPolygonsToAttach = polygonsToAttach2
}
