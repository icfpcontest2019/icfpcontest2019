package lambda.geometry

/**
  * Turns
  */
object Turn extends Enumeration {
  type Turn = Value
  val RightTurn, LeftTurn, NoTurn = Value

  implicit def _turn2Int(t: Turn): Int = t match {
    case RightTurn => 1
    case LeftTurn => -1
    case NoTurn => 0
  }

  implicit def _turn2Bool(t: Turn): Boolean = t match {
    case RightTurn => true
    case LeftTurn => false
    case NoTurn => throw GeometryException("Cannot convert NoTurn to Boolean", None)
  }

}
