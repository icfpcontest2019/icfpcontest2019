package lambda.geometry.floating.generators.old

import lambda.geometry.floating.FPolygon
import lambda.geometry.floating.examples.FPolygonExamples.simple3Rectangle
import lambda.geometry.floating.generators.PolygonGenerator
import org.scalacheck.Gen

/**
  * @author Ilya Sergey
  */
object QuasiConvexPolygonGenerator extends PolygonGenerator {
  override val myOutFileName: String = "qconvex.pol"
  override val myPolygonsToAttach = convexPolygons
  override val myBasePolygon: FPolygon = simple3Rectangle
  override val myFreqs: Seq[Int] = freqs3
  override val myPolygonSize: Gen[Int] = Gen.choose(2, 6)
  override val myPositionStrategy = posStrategy1
  override val myGenerations: Int = 20
}
