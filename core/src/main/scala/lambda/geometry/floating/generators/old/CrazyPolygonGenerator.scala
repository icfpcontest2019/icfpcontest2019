package lambda.geometry.floating.generators.old

import lambda.geometry.floating.FPolygon
import lambda.geometry.floating.examples.FPolygonExamples.simple3Rectangle
import lambda.geometry.floating.generators.PolygonGenerator
import org.scalacheck.Gen

/**
  * @author Ilya Sergey
  */
object CrazyPolygonGenerator extends PolygonGenerator {
  override val myOutFileName: String = "crazy.pol"
  override val myBasePolygon: FPolygon = simple3Rectangle
  override val myPolygonsToAttach = polygonsToAttach
  override val myFreqs: Seq[Int] = freqs4
  override val myPolygonSize: Gen[Int] = Gen.choose(2, 6)
  override val myPositionStrategy = posStrategy1
  override val myGenerations: Int = 80
}
