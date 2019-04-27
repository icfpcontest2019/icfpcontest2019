package lambda.misc.artgallery

import lambda.geometry.floating
import lambda.geometry.floating.{FPoint, FPolygon}
import lambda.misc.rendering.BasicGeometryParsers

/*
For the Guards problem (I), the format for input polygon lines is as follows:

Num : Vertices

For instance, the following is polygon number 17 with 6 vertices:

17: (0, 0), (5, 0), (5, 2), (4, 1), (1, 1), (0, 2)

 */

object GuardInputParser extends BasicGeometryParsers {
  def line: Parser[(Int, FPolygon)] = (wholeNumber ~ ":") ~ points ^^ {
    case num ~ _ ~ vs => (num.toInt, floating.FPolygon(vs))
  }

  def apply(s: String) = parseAll(line, s)
}

/*
For the Guards problem (I), the format for solution file lines is as follows:

Num : Guards

For instance, the following is polygon number 17 with 3 suggested guards

17: (0, 0), (1, 1), (5, 2)
 */

object GuardSolutionParser extends BasicGeometryParsers {
  def line: Parser[(Int, Seq[FPoint])] = (wholeNumber <~ ":") ~ points ^^ {
    case num ~ guards => (num.toInt, guards)
  }

  def apply(s: String) = parseAll(line, s)
}

/*
Parsing serialized results in the format

 pID : guardsSize / numOfVertices

 For instance,

3: 5 / 16
 */
object GuardResultParser extends BasicGeometryParsers {
  def line: Parser[(Int, Int)] = (wholeNumber <~ ":") ~ wholeNumber <~ ("/" ~ wholeNumber) ^^ {
    case pNum ~ gNum => (pNum.toInt, gNum.toInt)
  }

  def apply(s: String) = parseAll(line, s)
}

/**
  * Parsing polygons/guards to check
  */

/*
For the Check problem (II), the format for input polygon lines is as follows:

Num : Vertices ; Guards

For instance, the following is polygon number 17 with 6 vertices and 1 guard

17: (0, 0), (5, 0), (5, 2), (4, 1), (1, 1), (0, 2) ; (4, 1)

 */
object CheckInputParser extends BasicGeometryParsers {
  def line: Parser[(Int, (FPolygon, Seq[FPoint]))] = (wholeNumber <~ ":") ~ points ~ (";" ~> points) ^^ {
    case num ~ vs ~ gs => (num.toInt, (floating.FPolygon(vs), gs))
  }

  def apply(s: String) = parseAll(line, s)
}


/*
For the Check problem (II), the format for solution file lines is as follows:

Num : Point

For instance, the following is a proposed thief position for polygon 17

17: (0.1, 0.1)


 */

object CheckSolutionParser extends BasicGeometryParsers {
  def line: Parser[(Int, FPoint)] = (wholeNumber <~ ":") ~ point ^^ {
    case num ~ pt => (num.toInt, pt)
  }

  def apply(s: String) = parseAll(line, s)
}

// The result parser is the same as for the solution parser
object CheckResultParser extends BasicGeometryParsers {
  def apply(s: String) = CheckSolutionParser.apply(s)
}


