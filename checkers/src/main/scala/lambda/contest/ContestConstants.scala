package lambda.contest

/**
  * @author Ilya Sergey
  */
object ContestConstants {

  import TaskDataTypes._

  // Booster symbols
  val BATTERIES_BOOSTER = 'B'
  val COFFEE_BOOSTER = 'F'
  val DRILL_BOOSTER = 'I'
  val PORTAL_BOOSTER = 'P'
  val CALL_FRIEND_BOOSTER = 'W'
  val CALL_POSITION = 'C'

  def codeToBooster(c: Char): Booster = c match {
    case BATTERIES_BOOSTER => Batteries
    case COFFEE_BOOSTER => Coffee
    case DRILL_BOOSTER => Drill
    case PORTAL_BOOSTER => Portal
    case CALL_FRIEND_BOOSTER => CallFriend
    case CALL_POSITION => CallPoint
  }

}
