package lambda.contest.parsers

import lambda.contest.Booster
import lambda.contest.ContestConstants._
import lambda.geometry.GeometryParsers

/**
  * @author Ilya Sergey
  */
object BoosterBuyingParser extends GeometryParsers {

  private def boosterParser(c: Char): Parser[Booster.Value] =
    s"$c" ^^ (b => codeToBooster(b.charAt(0)))


  private def boostersAlt: Parser[(Booster.Value)] =
    boosterParser(DRILL_LETTER) |
      boosterParser(INSTALL_TELEPORT_LETTER) |
      boosterParser(ARM_LETTER) |
      boosterParser(WHEELS_LETTER) |
      boosterParser(CALL_FRIEND_LETTER)


  def boosterBought: Parser[List[Booster.Value]] = rep(boostersAlt)

  def apply(s: String): ParseResult[List[Booster.Value]] = parseAll(boosterBought, s)

}
