package lambda.geometry.generators

import java.util.Random

import lambda.geometry.{Point2D, PointPolar, Polygon}
import org.scalacheck.{Gen, Shrink}
import lambda.geometry.CommonUtils._
import lambda.geometry._
import lambda.geometry.generators.PolygonGenerators._

/**
  * @author Ilya Sergey
  */

object PolygonPropertyUtils {


  /**
    * Construct an aligned rectangular polygon with the bottom-right
    * angle in the section
    */
  val generateNormalizedRectangle: LazyPolygon =
    (longEdgeSize: Int) => {
      val i = randomIntBetween(1, longEdgeSize)
      val j = randomIntBetween(3, longEdgeSize * 3)
      Polygon(Seq(
        Point2D(0, 0), Point2D(0, j),
        Point2D(-i, j), Point2D(-i, 0)))
    }

  val generate3Rectangle: LazyPolygon =
    (longEdgeSize: Int) =>
      Polygon(Seq(
        Point2D(0, 0), Point2D(0, longEdgeSize),
        Point2D(-3, longEdgeSize), Point2D(-3, 0)))


  def polyFreqs[T](fs: Seq[Int], ps: Seq[T]): Gen[T] =
    Gen.frequency(fs.zip(ps.map(p => Gen.const(p))): _*)

  val polygonCombinatorCollector = (pc: CompositePolygon) => {
    val n = pc.size
    if (n <= 60) s"Small-size polygon (<= 60 vertices)"
    else if (n > 60 && n <= 150) s"Medium-size polygon (61..150 vertices)"
    else if (n > 150 && n <= 300) s"Large-size polygon (151..300 vertices)"
    else s"Very large-size polygon (>300 vertices)"
  }

  def generateConvexPolygon(n: Int): Polygon = {
    assert(n > 2)
    val frac = PI2 / n
    val vs: Seq[Point2D] = for (i <- 0 until n; phi = i * frac) yield PointPolar(1, phi).toCart
    Polygon(vs)
  }

  implicit val shrinkPolygonCombinator: Shrink[CompositePolygon] =
    Shrink { (p: CompositePolygon) =>
      PolygonCombinatorUtils.makeTraversals(p)
    }
}