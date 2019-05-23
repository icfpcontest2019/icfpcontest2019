package lambda.solvers

import lambda.contest.ContestConstants.{Action, MoveDown, MoveLeft, MoveRight, MoveUp, Snooze}
import lambda.contest.ContestException
import lambda.geometry.integer.IPoint

/**
  * @author Ilya Sergey
  */
object SolverUtils {

  def convertPointsToMoves(sol: List[IPoint]): List[List[Action]] = {
    val steps = getPairs(sol).toList
    val moves = steps.map(pairToAction) //.filter(_ != Snooze)
    List(moves)

  }

  def pairToAction(p: (IPoint, IPoint)): Action = p match {
    case (IPoint(ax, ay), IPoint(bx, by)) =>
      val dx = bx - ax
      val dy = by - ay
      (dx, dy) match {
        case (1, 0) => MoveRight
        case (-1, 0) => MoveLeft
        case (0, 1) => MoveUp
        case (0, -1) => MoveDown
        case (0, 0) => Snooze
        case _ => throw ContestException(s"Bad transition: ${(ax, ay)} to ${(bx, by)}")
      }
  }


  def getPairs[T](vs: Seq[T]): Seq[(T, T)] =
    if (vs.size <= 1) Nil
    else {
      val n = vs.size
      for (i <- 1 until n) yield (vs(i - 1), vs(i))
    }





}
