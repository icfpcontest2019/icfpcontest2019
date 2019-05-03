package lambda.contest

/**
  * @author Ilya Sergey
  */
object ContestConstants {

  /* *************************************** */
  //             Booster symbols
  /* *************************************** */

  val UP_LETTER = 'W'
  val DOWN_LETTER = 'S'
  val LEFT_LETTER = 'A'
  val RIGHT_LETTER = 'D'
  val SNOOZE_LETTER = 'Z'
  val TURN_LEFT_LETTER = 'Q'
  val TURN_RIGHT_LETTER = 'E'

  val BATTERIES_LETTER = 'B'
  val COFFEE_LETTER = 'F'
  val DRILL_LETTER = 'I'
  val TELEPORT_LETTER = 'T'
  val CALL_FRIEND_LETTER = 'C'
  val CALL_POSITION_LETTER = 'X'

  lazy val CONTEST_LETTERS = List(
    BATTERIES_LETTER,
    COFFEE_LETTER,
    DRILL_LETTER,
    TELEPORT_LETTER,
    CALL_FRIEND_LETTER,
    CALL_POSITION_LETTER,
    UP_LETTER,
    DOWN_LETTER,
    LEFT_LETTER,
    RIGHT_LETTER,
    SNOOZE_LETTER,
    TURN_LEFT_LETTER,
    TURN_RIGHT_LETTER,
  )

  import Booster._

  def codeToBooster(c: Char): Booster.Value = c match {
    case BATTERIES_LETTER => BatteriesBooster
    case COFFEE_LETTER => CoffeeBooster
    case DRILL_LETTER => DrillBooster
    case TELEPORT_LETTER => TeleportBooster
    case CALL_FRIEND_LETTER => CallWatchmanBooster
    case CALL_POSITION_LETTER => CallPoint
  }

  /* *************************************** */
  //            Solution symbols
  /* *************************************** */

  abstract class Move(char: Char)


  // Movements
  case object MoveUp extends Move(UP_LETTER)

  case object MoveDown extends Move(DOWN_LETTER)

  case object MoveLeft extends Move(LEFT_LETTER)

  case object MoveRight extends Move(RIGHT_LETTER)

  case object TurnLeft extends Move(TURN_LEFT_LETTER)

  case object TurnRight extends Move(TURN_RIGHT_LETTER)

  case object Snooze extends Move(SNOOZE_LETTER)

  // Boosters
  case class UseBatteries(dx: Int, dy: Int) extends Move(BATTERIES_LETTER)

  case object UseCoffee extends Move(COFFEE_LETTER)

  case object UseDrill extends Move(DRILL_LETTER)

  case class UseTeleport(x: Int, y: Int) extends Move(TELEPORT_LETTER)

  case object UseCallFriend extends Move(CALL_FRIEND_LETTER)

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
