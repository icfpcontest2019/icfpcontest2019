package lambda.misc.rendering

import lambda.geometry.GeometryParsers
import lambda.geometry.floating.FPoint

/**
  * @author Ilya Sergey
  */
object ArtGalleryParser extends GeometryParsers {

  def parts: Parser[(Seq[FPoint], Seq[FPoint], Boolean)] = {
    val ps = points(floatPoint)
    ps ~ opt(rep1(sepToken) ~ ps) ~ opt(rep1(sepToken) ~ boolParser) ^^ {
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
  }

  def apply(s: String) = parseAll(parts, s)
}
