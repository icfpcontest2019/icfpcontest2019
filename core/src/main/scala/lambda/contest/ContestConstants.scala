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
  val INTSTALL_TELEPORT_LETTER = 'T'
  val DO_TELEPORT_LETTER = 'P'
  val CALL_FRIEND_LETTER = 'C'
  val CALL_POINT_LETTER = 'X'

  lazy val CONTEST_LETTERS = List(
    BATTERIES_LETTER,
    COFFEE_LETTER,
    DRILL_LETTER,
    INTSTALL_TELEPORT_LETTER,
    DO_TELEPORT_LETTER,
    CALL_FRIEND_LETTER,
    CALL_POINT_LETTER,
    UP_LETTER,
    DOWN_LETTER,
    LEFT_LETTER,
    RIGHT_LETTER,
    SNOOZE_LETTER,
    TURN_LEFT_LETTER,
    TURN_RIGHT_LETTER,
  )

  import Booster._

  val Boosters = List(BatteriesBooster, CoffeeBooster, DrillBooster, TeleportBooster, CallWatchmanBooster, CallPoint)

  def codeToBooster(c: Char): Booster.Value = c match {
    case BATTERIES_LETTER => BatteriesBooster
    case COFFEE_LETTER => CoffeeBooster
    case DRILL_LETTER => DrillBooster
    case INTSTALL_TELEPORT_LETTER => TeleportBooster
    case CALL_FRIEND_LETTER => CallWatchmanBooster
    case CALL_POINT_LETTER => CallPoint
  }

  /* *************************************** */
  //            Solution symbols
  /* *************************************** */

  sealed abstract class Action(char: Char) {
    def isMove: Boolean = false
  }

  sealed trait UseBooster


  // Movements
  case object MoveUp extends Action(UP_LETTER) {
    override def isMove = true
  }

  case object MoveDown extends Action(DOWN_LETTER) {
    override def isMove = true
  }

  case object MoveLeft extends Action(LEFT_LETTER) {
    override def isMove = true
  }

  case object MoveRight extends Action(RIGHT_LETTER) {
    override def isMove = true
  }

  // Rotations and skip

  case object TurnLeft extends Action(TURN_LEFT_LETTER)
  case object TurnRight extends Action(TURN_RIGHT_LETTER)
  case object Snooze extends Action(SNOOZE_LETTER)

  // Using boosters

  case class UseBatteries(dx: Int, dy: Int) extends Action(BATTERIES_LETTER) with UseBooster
  case object UseCoffee extends Action(COFFEE_LETTER) with UseBooster
  case object UseDrill extends Action(DRILL_LETTER) with UseBooster
  case object InstallTeleport extends Action(INTSTALL_TELEPORT_LETTER) with UseBooster
  case object UseCallFriend extends Action(CALL_FRIEND_LETTER) with UseBooster

  // Teleporting
  case class DoTeleport(x: Int, y: Int) extends Action(DO_TELEPORT_LETTER)

  /* *************************************** */
  //            Booster constants            //
  /* *************************************** */

  val COFFEE_TIME = 50
  val DRILL_TIME = 20

  /* *************************************** */
  //            Default torch range          //
  /* *************************************** */

  def DEFAULT_CONTEST_TORCH = List((1, 1), (1, 0), (1, -1))
  def SQUARE_TORCH = List((1, 1), (1, 0), (1, -1), (0, 1), (0, -1), (-1, 1), (-1, 0), (-1, -1))


}
