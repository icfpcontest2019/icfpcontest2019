package lambda.geometry.integer

import lambda.geometry.GeometryParsers

/**
  * @author Ilya Sergey
  */
object IPolygonUtils {

  def parsePoly(s: String): IPolygonParser.ParseResult[IPolygon] = IPolygonParser(s)
  
}

object IPolygonParser extends GeometryParsers {
  def apply(s: String) = parseAll(ipoly, s)
}
  
