package lambda.contest.checker

import lambda.contest.checkers.GraderUtils.readOneTask
import lambda.contest.checkers.IndividualGrader.readSolutionsAndBoosters
import lambda.contest.checkers.TaskExecution
import lambda.contest.{Booster, ContestConstants, ContestException}
import org.scalatest.{FlatSpec, Matchers}

/**
  * @author Ilya Sergey
  */
class SolutionCheckerTests extends FlatSpec with Matchers {

  type SolMap = Map[Int, (List[Booster.Value], List[List[ContestConstants.Action]])]
  
  val toyTasks = "./src/test/resources/contest/toy/problems"
  val toySolutions = "./src/test/resources/contest/toy/teams/hare/2019-05-12-12-01-01"
  var toySolutionMap = readSolutionsAndBoosters(toySolutions)

  s"A checker" should s"succeed for toy tasks" in { }

  for (i <- toySolutionMap.keySet.toList.sorted) {
    checkSolution(i, toyTasks, toySolutionMap)
  }

  ///////////////////////////////////////////////////////////////////////////
  
  private val contestTasksPath = "./src/main/resources/contest/final"
  private val contestSolutionsPath = "./src/main/resources/contest/solutions"
  val contestSolutionMap: SolMap = readSolutionsAndBoosters(contestSolutionsPath)
  
  
  
  s"A checker" should s"succeed for all 300 tasks" in {}

  for (i <- 1 to 300) {
    checkSolution(i, contestTasksPath, contestSolutionMap)
  }


  ////////////////////////////////////////////////////////////////////////////

  def checkSolution(taskN: Int, taskPath: String, solMap: SolMap): Unit = {
    val failMsg = s"Failure while checking the task $taskN."
    val t0 = System.currentTimeMillis()
    it should s"pass for task $taskN [$taskPath]" in {
      val task = try {
        readOneTask(taskPath, taskN)
      } catch {
        case e: Throwable => None
      }
      task match {
        case Some((matrix, dx, dy, init)) =>
          val (boosters, moves) = solMap(taskN)
          val state = TaskExecution.createState(matrix, dx, dy, init, moves, boosters)
          val res = try {
            state.evalSolution() match {
              case Some(_) =>
              case None =>
                System.err.println(failMsg)
                assert(false)
            }
          } catch {
            case ContestException(loc, _) =>
              val msg = s"Failure in task $taskN: $loc."
              System.err.println(msg)
              assert(false)
            case _: Throwable =>
              assert(false)
          }
        case None =>
          System.err.println(s"A result is expected for task $taskN.")
          assert(false)
      }
    }
  }
  

}