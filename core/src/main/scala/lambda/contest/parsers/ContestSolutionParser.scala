package lambda.contest.parsers

import lambda.contest.ContestConstants._
import lambda.geometry.GeometryParsers
import lambda.geometry.integer.IPoint

/**
  * @author Ilya Sergey
  */
object ContestSolutionParser extends GeometryParsers {

  // TODO: Implement me

  def moveParser: Parser[Move] =
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
      | TELEPORT_LETTER ~ intPoint ^^ { case _ ~ IPoint(x, y) => UseTeleport(x, y) }
      | CALL_FRIEND_LETTER ^^^ UseCallFriend)

  ((BATTERIES_LETTER: Parser[Char]) | COFFEE_LETTER | DRILL_LETTER |
    TELEPORT_LETTER | CALL_FRIEND_LETTER | CALL_POINT_LETTER) ^^ codeToBooster

  def moves: Parser[List[Move]] = rep(moveParser)

  def solutionParser: Parser[List[List[Move]]] = rep1sep(moves, sepToken)

  def apply(s: String) = parseAll(solutionParser, s)


}
