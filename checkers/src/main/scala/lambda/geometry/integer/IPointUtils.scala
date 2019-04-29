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
  def direction(p0: IPoint, p1: IPoint, p2: IPoint): Turn = {
    val sig = scala.math.signum(crossProduct(p2 - p0, p1 - p0))
    sig match {
      case 1 => RightTurn
      case -1 => LeftTurn
      case 0 => NoTurn
    }
  }
}

