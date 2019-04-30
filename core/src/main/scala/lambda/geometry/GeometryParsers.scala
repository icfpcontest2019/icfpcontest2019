package lambda.geometry

import lambda.geometry.floating.FPoint
import lambda.geometry.integer.{IPoint, IPolygon}

import scala.util.parsing.combinator.JavaTokenParsers

/**
  * @author Ilya Sergey
  */
trait GeometryParsers extends JavaTokenParsers {
  val semicolon= ";"
  val sepToken = "#"

  def boolParser: Parser[Boolean] = ("true" ||| "false") ^^ (_.toBoolean)

  def points[T](pointParser: Parser[T]): Parser[Seq[T]] = rep1sep(pointParser, ",")

  /**
    * Parse 2D point with floating point coordinates
    */
  def floatPoint: Parser[FPoint] = "(" ~ floatingPointNumber ~ ("," ~> floatingPointNumber) ~ ")" ^^ {
    case _ ~ f1 ~ f2 ~ _ => FPoint(f1.toDouble, f2.toDouble)
  }

  /**
    * Parse 2D point with integerpoint coordinates
    */
  def intPoint: Parser[IPoint] = "(" ~ wholeNumber ~ ("," ~> wholeNumber) ~ ")" ^^ {
    case _ ~ x ~ y ~ _ => IPoint(x.toInt, y.toInt)
  }

  def ipoly: Parser[IPolygon] = points(intPoint) ^^ IPolygon

}
