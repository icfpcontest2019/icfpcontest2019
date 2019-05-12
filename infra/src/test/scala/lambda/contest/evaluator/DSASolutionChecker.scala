package lambda.contest.evaluator

import java.io.File

import lambda.contest.ContestConstants._
import lambda.contest.checkers.TaskCreationUtils.contestTaskToMatrix
import lambda.contest.checkers.TaskExecution
import lambda.contest.parsers.ContestTaskParser
import lambda.contest.{ContestException, ContestTask}
import lambda.geometry.GeometryParsers
import lambda.geometry.floating.FPolygonUtils
import lambda.geometry.integer.IPoint
import lambda.util.FileUtil
import lambda.util.stats.StatisticsUtils
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable.{Map => MMap}

/**
  * @author Ilya Sergey
  */


class DSASolutionChecker extends FlatSpec with Matchers
  with DSASolutions {
  
  val (tNum, tasks) = getTeamsAndTasks

  s"An evaluator for tasks" should "work correctly on all 10 rooms for teams solutions" in {}
  for (i <- 1 to tNum) {
    checkTeam(i)
  }
  
  
  def checkTeam(i: Int) = {
    it should s"make all solutions of team $i pass" in {
      println("----- Team 1 -----")
      val teamSolution = readSolutionsForTeam(i)
      checkTeamSolutions(tasks, i, teamSolution)
      println()
    }
  }
  
}

trait DSASolutions extends StatisticsUtils[Int, Unit] {

  val dsaTaskPath = s"${System.getProperty("user.dir")}/src/test/resources/simple/rooms/"

  val dsaSolutionPath = s"${System.getProperty("user.dir")}/src/test/resources/simple/solutions/"

  val torchShape = List((1, 1), (1, 0), (1, -1), (0, 1), (0, -1), (-1, 1), (-1, 0), (-1, -1))

  def checkTeams = {
    val (tnum: Int, tasks: Map[Int, ContestTask]) = getTeamsAndTasks

    var scores: ResultsToRank = Map.empty
    var intScores: Map[Int, Map[Int, Int]] = Map.empty

    for (i <- 1 to tnum) {
      val teamSolution = readSolutionsForTeam(i)
      println(s"--- Team $i ---")
      val sol = checkTeamSolutions(tasks, i, teamSolution)
      intScores = intScores + (i -> sol.toMap)

      val teamSolMap = sol.map { case (task, score) => (task, Some(score.toDouble)) }.toMap
      scores = scores + (i -> ((), teamSolMap))

      println()
    }
    val ranking = rankTeams(scores, 1, true)
    (tasks, intScores)
  }
  
  def getTeamsAndTasks = {
    val teamDir = new File(dsaSolutionPath)
    assert(teamDir.isDirectory)
    val tnum = teamDir.listFiles().toList.count(_.getName.startsWith("team"))
    val tasks = readTasks

    for (t <- tasks.keys.toList.sorted) {
      val task = tasks(t)
      val (x, y) = task.room.dimensions
      println(s"Task $t: bounding box: $x x $y, area: ${FPolygonUtils.computeArea(task.room.toFPolygon).toInt}")
    }
    println()
    (tnum, tasks)
  }

  def printToCSV(intScores: Map[Int, Map[Int, Int]], problemNum: Int) = {
    val titles = ("Team" :: (1 to problemNum).map(_.toString).toList).mkString(", ")
    val lines = for (i <- intScores.keySet.toList.sorted) yield {
      val results = intScores(i).toList.sortBy(_._1).map(_._2)
      (i :: results).mkString(", ")
    }

    val file = "results.csv"
    FileUtil.writeToNewFile(file, (titles :: lines).mkString("\n"))

  }


  //////////////////////////////////////////
  //             Checking
  //////////////////////////////////////////
  def checkTeamSolutions(tasks: Map[Int, ContestTask],
                         tNum: Int,
                         teamSolutions: Map[Int, List[List[Action]]]): List[(Int, Int)] = {
    val resultMap: MMap[Int, Int] = MMap.empty

    for (i <- tasks.keySet.toList.sorted) {
      val task = tasks(i)
      val sol = teamSolutions(i)
      val (matrix, mx, my) = contestTaskToMatrix(task)
      val state = TaskExecution.createState(matrix, mx, my, task.initPos, sol, torchShape)
      val res = state.evalSolution(Nil)
      // println(state.toStringBuffer)

      assert(res.isDefined)
      val numSteps = res.get
      resultMap += i -> numSteps
      println(s"  Room $i: $numSteps")

    }
    resultMap.toList
  }

  //////////////////////////////////////////
  //                Tasks
  //////////////////////////////////////////
  private def readTasks: Map[Int, ContestTask] = {
    (1 to 10).foldLeft(Map.empty[Int, ContestTask]) { case (m, i) =>
      val taskLine = FileUtil.readFromFile(s"$dsaTaskPath/room$i").head
      val task: ContestTask = ContestTaskParser(taskLine).get
      m + (i -> task)
    }
  }

  //////////////////////////////////////////
  //                Solutions
  //////////////////////////////////////////

  object DSAParser extends GeometryParsers {
    def lineParser: Parser[(Int, List[IPoint])] =
      (wholeNumber <~ ":") ~ rep1sep(intPoint, ";") ^^ { case n ~ ps => (n.toInt, ps) }

    def apply(s: String) = parseAll(lineParser, s)
  }

  def readSolutionsForTeam(i: Int): Map[Int, List[List[Action]]] = {
    val lines = FileUtil.readFromFile(s"$dsaSolutionPath/team$i/sol.txt")
    val trimmed = lines.filter(_.trim.nonEmpty)
    val dsaSolutions = trimmed.map(DSAParser(_).get)
    val ls = for ((k, s) <- dsaSolutions) yield k -> convertDSASolution(s)
    ls.toMap
  }

  private def convertDSASolution(sol: List[IPoint]): List[List[Action]] = {
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


object DSARunner extends DSASolutions with App {
  val checkTeamsResult: (Map[Int, ContestTask], Map[Int, Map[Int, Int]]) = checkTeams
  val tasks: Map[Int, ContestTask] = checkTeamsResult._1
  var intScores: Map[Int, Map[Int, Int]] = checkTeamsResult._2

  printToCSV(intScores, tasks.size)
}