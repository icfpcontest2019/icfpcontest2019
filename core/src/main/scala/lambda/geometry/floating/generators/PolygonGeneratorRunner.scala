package lambda.geometry.floating.generators

import java.io.File

import lambda.geometry._
import lambda.geometry.floating.FPolygon
import lambda.geometry.floating.examples.FPolygonExamples._
import lambda.geometry.floating.generators.PolygonGenerators._
import lambda.geometry.floating.generators.PolygonPropertyUtils._
import lambda.geometry.floating.generators.old.RandomCrazyPolygonGenerator
import lambda.util.FileUtil.writeToNewFile
import org.scalacheck.Gen

/**
  * @author Ilya Sergey
  */

trait GeneratorPrimitives {
  val freqs = Seq(10, 1, 2, 2, 2, 2)
  val freqs4 = Seq(4, 4, 2, 2, 2, 2)

  val freqs2 = Seq(10,2)

  val polygonsToAttach = Seq(
    generateNormalizedRectangle,
    prep(chvatal_comb1),
    prep(kittyPolygon),
    prep(triangle3),
    prep(triangle2),
    prep(generateConvexPolygon(12))
  )

  val polygonsToAttach2 = Seq(generate3Rectangle, generateNormalizedRectangle)

  val convexPolygons = Seq(prepNoScale(generateConvexPolygon(20)))
  val polygonsToAttach3 = Seq(prep(triangle2))

  val freqs3 = Seq(8, 2)
  val posStrategy0 = (l: Double) => Some((0, l.toInt))
  val posStrategy1 = RandomCrazyPolygonGenerator.positionStrategy

  val posStrategy2 = (l: Double) => {
    if (l < 3) None
    else Some((1, 2))
  }

  val posStrategy3 = (l: Double) => {
    if (l.toInt < 3) None
    else {
      val startOffset = randomIntBetween(1, l.toInt - 1)
      val endOffset = randomIntBetween(startOffset + 1, l.toInt)
      Some((startOffset, endOffset))
    }
  }


}

trait PolygonGenerator extends GeneratorPrimitives {

  // Polygon-specific fields

  val myOutFileName: String
  val myBasePolygon: FPolygon
  val myFreqs: Seq[Int]
  val myGenerations: Int
  val myPolygonsToAttach : Seq[LazyPolygon]
  val myPolygonSize: Gen[Int]
  val myPositionStrategy: Double => Option[(Int, Int)]

  /**
    * Useful definitions
    */
  def generatePolygons(num: Int, filePath: String) = {
    val pols = for (i <- 1 to num) yield {
      val pc: CompositePolygon =
        generateStuff(myPolygonsToAttach, myFreqs, myBasePolygon, myPositionStrategy, myGenerations)
      s"$i: ${pc.pol.toString}"
    }

    val strs = pols.mkString(System.lineSeparator())
    writeToNewFile(filePath, strs)
  }

  def generateStuff(polygonsToAttach: Seq[LazyPolygon], freqs: Seq[Int],
                    base: FPolygon, posStrategy: Double => Option[(Int, Int)],
                    gens: Int): CompositePolygon = {
    val attachments: Gen[LazyPolygon] =
      Gen.frequency(freqs.zip(polygonsToAttach.map(p => Gen.const(p))): _*)
    generatePolygon(base, attachments, Gen.choose(2, 6), gens, posStrategy)
  }


  def main(args: Array[String]) {
    val outPath =
      if (args.length > 0) args(0)
      else Seq(System.getProperty("user.dir"), myOutFileName).mkString(File.separator)

    val num = if (args.length > 1) Integer.parseInt(args(1)) else 30
    generatePolygons(num, outPath)
  }

}








