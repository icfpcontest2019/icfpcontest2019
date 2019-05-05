package lambda.geometry.integer

import lambda.geometry.Turn

/**
  * @author Ilya Sergey
  */
object IPointUtils {

  import Turn._

  val origin2D = IPoint(0, 0)


  /**
    * Cross-product of three points on a plane
    */

  def crossProduct(p1: IPoint, p2: IPoint): Int =
    p1.x * p2.y - p2.x * p1.y

  def dotProduct(p1: IPoint, p2: IPoint) =
    p1.x * p2.x + p1.y * p2.y


  /**
    * Turn between three points
    * +1 -- right turn
    * -1 -- left turn
    * 0 -- points are collinear
    */
  def direction(pi: IPoint, pj: IPoint, pk: IPoint): Turn = {
    val sig = scala.math.signum(crossProduct(pk - pi, pj - pi))
    sig match {
      case 1 => RightTurn
      case -1 => LeftTurn
      case 0 => NoTurn
    }
  }
  
  def squareTouchesOtherSquares(sq: IPolygon, other: List[IPolygon]) : Boolean = {
    if (other.contains(sq)) return false
    
    val esReverted = sq.edges.map {case ISegment(a, b) => ISegment(b, a)}
    for {
      os <- other
      e2 <- os.edges
      e1 <- esReverted
    } {
      if (e1 == e2) return true
    }
    false
  }
}

