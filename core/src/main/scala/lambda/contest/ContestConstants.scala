package lambda.contest

/**
  * @author Ilya Sergey
  */
object ContestConstants {

  /* *************************************** */
  //             Booster symbols
  /* *************************************** */

  val BATTERIES_BOOSTER = 'B'
  val COFFEE_BOOSTER = 'F'
  val DRILL_BOOSTER = 'I'
  val PORTAL_BOOSTER = 'P'
  val CALL_FRIEND_BOOSTER = 'W'
  val CALL_POSITION = 'C'

  import Booster._

  def codeToBooster(c: Char): Booster.Value = c match {
    case BATTERIES_BOOSTER => BatteriesBooster
    case COFFEE_BOOSTER => CoffeeBooster
    case DRILL_BOOSTER => DrillBooster
    case PORTAL_BOOSTER => PortalBooster
    case CALL_FRIEND_BOOSTER => CallFriendBooster
    case CALL_POSITION => CallPoint
  }

  /* *************************************** */
  //            Solution symbols
  /* *************************************** */

  abstract class Move(char: Char)

  // Movements
  case object Up extends Move('U')

  case object Down extends Move('D')

  case object Left extends Move('L')

  case object Right extends Move('R')

  case object Snooze extends Move('Z')

  case object TurnLeft extends Move('Q')

  case object TurnRight extends Move('E')

  // Boosters
  case class Batteries(dx: Int, dy: Int) extends Move('B')

  case object Coffee extends Move('F')

  case object Drill extends Move('I')

  case object Portal extends Move('P')

  case object CallFriend extends Move('W')

  /* *************************************** */
  //            Booster constants            //
  /* *************************************** */

  val COFFEE_TIME = 50
  val DRILL_TIME = 20


  /* *************************************** */
  //            Default torch range          //
  /* *************************************** */

  def DEFAULT_TORCH = List((1, 1), (1, 0), (1, -1))


}
