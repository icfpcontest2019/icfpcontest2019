package lambda.contest.parsers

import lambda.contest.{Booster, ContestTask}
import lambda.contest.ContestConstants._
import lambda.geometry.GeometryParsers
import lambda.geometry.integer.IPoint

/**
  * @author Ilya Sergey
  */
object ContestTaskParser extends GeometryParsers {

  def boosterParser: Parser[Booster.Value] =
    ((BATTERIES_LETTER: Parser[Char]) | COFFEE_LETTER | DRILL_LETTER |
      TELEPORT_LETTER | CALL_FRIEND_LETTER | CALL_POSITION_LETTER) ^^ codeToBooster

  def boosterPositionParser: Parser[List[(Booster.Value, IPoint)]] =
    repsep(boosterParser ~! intPoint ^^ { case b ~ p => (b, p) }, semicolon)

  /**
    * Parse complete task
    * - room (mandatory)
    * - obstacles (optional)
    * - boosters (optional)
    */
  def taskParser: Parser[ContestTask] =
    (ipoly ~ semicolon // Room
      ~ intPoint // Initial position
      ~ opt(semicolon ~> repsep(ipoly, semicolon)) // Obstacles
      ~ opt(semicolon ~> boosterPositionParser) // Booster position
      ^^ {
      case room ~ _ ~ initPos ~ obstacles ~ boosters =>
        ContestTask(room, initPos, obstacles.getOrElse(Nil), boosters.getOrElse(Nil))
    })

  def apply(s: String): ContestTaskParser.ParseResult[ContestTask] = parseAll(taskParser, s)

}
