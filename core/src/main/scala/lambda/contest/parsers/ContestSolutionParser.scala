package lambda.contest.parsers

import lambda.contest.ContestConstants._
import lambda.geometry.GeometryParsers
import lambda.geometry.integer.IPoint

/**
  * @author Ilya Sergey
  */
object ContestSolutionParser extends GeometryParsers {

  // TODO: Implement me

  def moveParser: Parser[Action] =
    (// Moves

      UP_LETTER ^^^ MoveUp
        | DOWN_LETTER ^^^ MoveDown
        | LEFT_LETTER ^^^ MoveLeft
        | RIGHT_LETTER ^^^ MoveRight
        | TURN_LEFT_LETTER ^^^ TurnLeft
        | TURN_RIGHT_LETTER ^^^ TurnRight
        | SNOOZE_LETTER ^^^ Snooze

      // Boosters
      | BATTERIES_LETTER ~ intPoint ^^ { case _ ~ IPoint(x, y) => UseBatteries(x, y) }
      | COFFEE_LETTER ^^^ UseCoffee
      | DRILL_LETTER ^^^ UseDrill
      | INTSTALL_TELEPORT_LETTER ^^^ InstallTeleport
      | DO_TELEPORT_LETTER ~ intPoint ^^ { case _ ~ IPoint(x, y) => DoTeleport(x, y) }
      | CALL_FRIEND_LETTER ^^^ UseCallFriend)

  def moves: Parser[List[Action]] = rep(moveParser)

  def solutionParser: Parser[List[List[Action]]] = rep1sep(moves, sepToken)

  def apply(s: String) = parseAll(solutionParser, s)


}
