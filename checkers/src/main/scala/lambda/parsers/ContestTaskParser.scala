package lambda.parsers

import lambda.contest.{Booster, ContestTask}
import lambda.contest.ContestConstants._
import lambda.geometry.GeometryParsers
import lambda.geometry.integer.IPoint

/**
  * @author Ilya Sergey
  */
object ContestTaskParser extends GeometryParsers {

  def boosterParser: Parser[Booster.Value] =
    ((BATTERIES_BOOSTER: Parser[Char]) | COFFEE_BOOSTER | DRILL_BOOSTER |
      PORTAL_BOOSTER | CALL_FRIEND_BOOSTER | CALL_POSITION) ^^ codeToBooster

  def boostersParser: Parser[Seq[(Booster.Value, IPoint)]] =
    repsep(boosterParser ~! intPoint ^^ { case b ~ p => (b, p) }, semicolon)

  /**
    * Parse complete task
    * - room (mandatory)
    * - obstacles (optional)
    * - boosters (optional)
    */
  def taskParser: Parser[ContestTask] =
    ipoly ~ opt(semicolon ~> repsep(ipoly, semicolon)) ~ opt(semicolon ~> boostersParser) ^^ {
      case room ~ obstacles ~ boosters =>
        ContestTask(room, obstacles.getOrElse(Nil), boosters.getOrElse(Nil))
    }

  def apply(s: String) = parseAll(taskParser, s)

}
