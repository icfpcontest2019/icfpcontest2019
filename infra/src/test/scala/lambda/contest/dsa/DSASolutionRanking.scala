package lambda.contest.dsa

import java.io.File

import lambda.contest.ContestConstants._
import lambda.contest.checkers.TaskCreationUtils.contestTaskToMatrix
import lambda.contest.checkers.TaskExecution
import lambda.contest.parsers.ContestTaskParser
import lambda.contest.{ContestException, ContestTask}
import lambda.geometry.GeometryParsers
import lambda.geometry.integer.IPoint
import lambda.util.FileUtil

import scala.collection.mutable.{Map => MMap}

/**
  * @author Ilya Sergey
  */
object DSASolutionRanking {

  val dsaTaskPath = "./infra/src/test/resources/simple/rooms/"
  val dsaSolutionPath = "./infra/src/test/resources/simple/solutions/"
  
  val torchShape = List((1, 1), (1, 0), (1, -1), (0, 1), (0, -1), (-1, 1), (-1, 0), (-1, -1)) 

  def main(args: Array[String]): Unit = {
    val teamDir = new File(dsaSolutionPath)
    assert(teamDir.isDirectory)
    val tnum = teamDir.listFiles().toList.count(_.getName.startsWith("team"))
    val tasks = readTasks
    
    for (i <- 1 to tnum) {
      val teamSolution = readSolutionsForTeam(i)
      println(s"--- Team $i ---")
      checkTeamSolutions(tasks, i, teamSolution)
      println()
      // TODO: Save results somewhere
    }
    
    
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
      assert(res.isDefined)
      val numSteps = res.get
      resultMap += tNum -> numSteps
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
    val moves = steps.map(pairToAction)
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
