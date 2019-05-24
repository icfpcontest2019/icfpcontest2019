//package lambda.js.parsers
//
//import lambda.contest.ContestConstants._
//import lambda.contest.{Booster, ContestTask}
//import lambda.geometry.integer.{IPoint, IPolygon}
//import fastparse._
//import JavaWhitespace._
//
//
///**
//  * @author Ilya Sergey
//  */
//object JSContestParserUtil {
//
//  def number[_: P]: P[Int] = P("-".? ~ CharIn("0-9").rep(1).!.map(_.toInt))
//
//  def point[_: P]: P[IPoint] = P("(" ~/ number ~ "," ~ number ~ ")").map { case (x, y) => IPoint(x, y) }
//
//  def poly[_: P]: P[IPolygon] = P(point.rep(sep = ",")).map(ps => IPolygon(ps.toList))
//  
//  def booster[_: P](c: Char): P[(Booster.Value, IPoint)] =
//    P(s"$c".! ~ point).map { case (b, p) => (codeToBooster(b.charAt(0)), p) }
//
//  def boosterAlt[_: P]: P[(Booster.Value, IPoint)] =
//    booster(DRILL_LETTER) |
//      booster(INSTALL_TELEPORT_LETTER) |
//      booster(BATTERIES_LETTER) |
//      booster(COFFEE_LETTER) |
//      booster(CALL_FRIEND_LETTER) |
//      booster(CALL_POINT_LETTER)
//
//  def task[_: P]: P[ContestTask] = P(poly ~ "#" ~ point ~ "#" ~ poly.rep(sep = ";") ~ "#" ~ boosterAlt.rep)
//    .map{ case (r, i, o, b) => ContestTask(r, i, o.toList, b.toList) }
//
//}
