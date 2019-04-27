package lambda.misc.rendering

import lambda.geometry.floating.FPoint

import scala.util.parsing.combinator.JavaTokenParsers

trait BasicGeometryParsers extends JavaTokenParsers {
  val sepToken = "#"

  def boolParser: Parser[Boolean] = ("true" ||| "false") ^^ {
    case s => s.toBoolean
  }

  def points: Parser[Seq[FPoint]] = rep1sep(point, ",")

  def point: Parser[FPoint] = "(" ~ floatingPointNumber ~ ("," ~> floatingPointNumber) ~ ")" ^^ {
      case _ ~ f1 ~ f2 ~ _ => FPoint(f1.toDouble, f2.toDouble)
    }

}

object PolygonParser extends BasicGeometryParsers {

  def parts: Parser[(Seq[FPoint], Seq[FPoint], Boolean)] = points ~ opt(rep1(sepToken) ~ points) ~ opt(rep1(sepToken) ~ boolParser) ^^ {
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
