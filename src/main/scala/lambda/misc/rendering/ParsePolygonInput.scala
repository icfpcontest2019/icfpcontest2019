package lambda.misc.rendering

import lambda.geometry.Point2D

import scala.util.parsing.combinator.JavaTokenParsers

trait BasicGeometryParsers extends JavaTokenParsers {
  val sepToken = "#"

  def boolParser: Parser[Boolean] = ("true" ||| "false") ^^ {
    case s => s.toBoolean
  }

  def points: Parser[Seq[Point2D]] = rep1sep(point, ",")

  def point: Parser[Point2D] = "(" ~ floatingPointNumber ~ ("," ~> floatingPointNumber) ~ ")" ^^ {
      case _ ~ f1 ~ f2 ~ _ => Point2D(f1.toDouble, f2.toDouble)
    }

}

object PolygonParser extends BasicGeometryParsers {

  def parts: Parser[(Seq[Point2D], Seq[Point2D], Boolean)] = points ~ opt(rep1(sepToken) ~ points) ~ opt(rep1(sepToken) ~ boolParser) ^^ {
    case p1 ~ p2 ~ b =>
      val c = b match {
        case Some(_ ~ g) => g;
        case _ => false
      }
      val zs = p2 match {
        case Some(_ ~ xs) => xs
        case _ => Seq.empty
      }
      (p1, zs, c)
  }

  def apply(s: String) = parseAll(parts, s)
}
