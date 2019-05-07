package lambda.geometry.floating.generators


import lambda.geometry._
import lambda.geometry.floating.generators.PolygonGenerators._
import lambda.geometry.floating.{FPoint, FPolygon, PointPolar, _}
import org.scalacheck.{Gen, Shrink}

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
      val j = randomIntBetween(3, longEdgeSize)
      FPolygon(Seq(
        FPoint(0, 0), FPoint(0, j),
        FPoint(-1, j), FPoint(-1, 0)))
    }

  val generateNormalizedSquare: LazyPolygon =
    (longEdgeSize: Int) => {
      val i = randomIntBetween(1, 1)
      val j = randomIntBetween(1, 1)
      FPolygon(Seq(
        FPoint(0, 0), FPoint(0, 1),
        FPoint(-1, 1), FPoint(-1, 0)))
    }

  val generate3Rectangle: LazyPolygon =
    (longEdgeSize: Int) =>
      FPolygon(Seq(
        FPoint(0, 0), FPoint(0, longEdgeSize),
        FPoint(-3, longEdgeSize), FPoint(-3, 0)))


  def polyFreqs[T](fs: Seq[Int], ps: Seq[T]): Gen[T] =
    Gen.frequency(fs.zip(ps.map(p => Gen.const(p))): _*)

  val polygonCombinatorCollector = (pc: CompositePolygon) => {
    val n = pc.size
    if (n <= 60) s"Small-size polygon (<= 60 vertices)"
    else if (n > 60 && n <= 150) s"Medium-size polygon (61..150 vertices)"
    else if (n > 150 && n <= 300) s"Large-size polygon (151..300 vertices)"
    else s"Very large-size polygon (>300 vertices)"
  }

  def generateConvexPolygon(n: Int): FPolygon = {
    assert(n > 2)
    val frac = PI2 / n
    val vs: Seq[FPoint] = for (i <- 0 until n; phi = i * frac) yield PointPolar(1, phi).toCart
    floating.FPolygon(vs)
  }

  implicit val shrinkPolygonCombinator: Shrink[CompositePolygon] =
    Shrink { (p: CompositePolygon) =>
      PolygonCombinatorUtils.makeTraversals(p)
    }
}



