package lambda.contest.parsers

import lambda.contest.ContestConstants._
import lambda.contest.{Booster, ContestTask}
import lambda.geometry.GeometryParsers
import lambda.geometry.integer.IPoint

/**
  * @author Ilya Sergey
  */
object ContestTaskParser extends GeometryParsers {

  def boosterParser(c: Char): Parser[(Booster.Value, IPoint)] =
    s"$c" ~ intPoint ^^ { case b ~ p => (codeToBooster(b.charAt(0)), p) }

  def variousBoosters: Parser[(Booster.Value, IPoint)] =
    boosterParser(DRILL_LETTER) |
      boosterParser(INTSTALL_TELEPORT_LETTER) |
      boosterParser(BATTERIES_LETTER) |
      boosterParser(COFFEE_LETTER) |
      boosterParser(CALL_FRIEND_LETTER) |
      boosterParser(CALL_POINT_LETTER)

  def boosterPositions: Parser[List[(Booster.Value, IPoint)]] =
    repsep(variousBoosters, semicolon)

  /**
    * Parse complete task
    * - room (mandatory)
    * - obstacles (optional)
    * - boosters (optional)
    */
  def taskParser: Parser[ContestTask] = {
    (ipoly ~ // Room
      sepToken ~ intPoint ~ // Initial position
      sepToken ~ repsep(ipoly, semicolon) ~ // Obstacles
      sepToken ~ boosterPositions // Boosters and their locations
      ^^ {
      case room ~ _ ~ initPos ~ _ ~ obstacles ~ _ ~ boosters =>
        ContestTask(room, initPos, obstacles, boosters)
    })
  }

  def apply(s: String): ContestTaskParser.ParseResult[ContestTask] = parseAll(taskParser, s)

}
