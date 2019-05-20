package lambda.contest.parsers

import lambda.contest.ContestConstants._
import lambda.geometry.GeometryParsers
import lambda.geometry.integer.IPoint

/**
  * @author Ilya Sergey
  */
object ContestSolutionParser extends GeometryParsers {

  // This is more flexible, as it allows arbitrary spaces b/w characters
  def asString(c: Char): Parser[Char] =
    s"$c" ^^ {s => s.charAt(0)}

  def moveParser: Parser[Action] =
    (// Moves

      asString(UP_LETTER) ^^^ MoveUp
        | asString(DOWN_LETTER) ^^^ MoveDown
        | asString(LEFT_LETTER) ^^^ MoveLeft
        | asString(RIGHT_LETTER) ^^^ MoveRight
        | asString(TURN_LEFT_LETTER) ^^^ TurnLeft
        | asString(TURN_RIGHT_LETTER) ^^^ TurnRight
        | asString(SNOOZE_LETTER) ^^^ Snooze

      // Boosters
      | asString(BATTERIES_LETTER) ~ intPoint ^^ { case _ ~ IPoint(x, y) => UseBatteries(x, y) }
      | asString(COFFEE_LETTER) ^^^ UseCoffee
      | asString(DRILL_LETTER) ^^^ UseDrill
      | asString(INSTALL_TELEPORT_LETTER) ^^^ InstallTeleport
      | asString(DO_TELEPORT_LETTER) ~ intPoint ^^ { case _ ~ IPoint(x, y) => DoTeleport(x, y) }
      | asString(CALL_FRIEND_LETTER) ^^^ UseCallFriend)

  def moves: Parser[List[Action]] = rep(moveParser)

  def solutionParser: Parser[List[List[Action]]] = rep1sep(moves, sepToken)

  def apply(s: String) = parseAll(solutionParser, s)


}
