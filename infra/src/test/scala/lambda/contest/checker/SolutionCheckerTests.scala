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

  private val contestTasksPath = "./src/main/resources/contest/final"
  private val contestSolutionsPath = "./src/main/resources/contest/solutions"

  var solutionMap: Map[Int, (List[Booster.Value], List[List[ContestConstants.Action]])] = Map.empty
    

  s"A checker" should s"succeed for all 300 tasks" in {
    solutionMap = readSolutionsAndBoosters(contestSolutionsPath)
  }

  def checkSolution(taskN: Int): Unit = {
    val failMsg = s"Failure while checking the task $taskN."
    val t0 = System.currentTimeMillis()
    it should s"pass for task $taskN" in {
      val task = try {
        readOneTask(contestTasksPath, taskN)
      } catch {
        case e: Throwable => None
      }
      task match {
        case Some((matrix, dx, dy, init)) =>
          val (boosters, moves) = solutionMap(taskN)
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
  
  for (i <- 1 to 300) {
    checkSolution(i)
  }

}