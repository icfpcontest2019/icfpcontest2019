package lambda.contest.evaluator

import lambda.contest.checkers.TaskExecution
import lambda.contest.examples.RoomsWithPartialSolutions
import lambda.contest.ContestException
import lambda.geometry.integer.IPoint
import lambda.util.FileUtil
import org.scalatest.{FlatSpec, Matchers}

/**
  * @author Ilya Sergey
  */
class SolutionPropertyTests extends FlatSpec with Matchers
  with RoomsWithPartialSolutions with SolutionCheckerUtils {

  private val solutionSuffix = ".sol"

  private def testWithExpected(state: TaskExecution, out: String, name: String): Unit = {
    println()
    println(name)
    println("Initial state:")
    println(state.toStringBuffer)
    state.evalSolution()
    val stateString = state.toStringBuffer.toString().trim

    println("Final state:")
    println(stateString)
    assertResult(out.trim)(stateString)
  }

  private def testShouldBreak(state: TaskExecution, whereBreaks: (Int, Int), name: String) {
    println()
    println(name)
    println("Initial state:")
    println(state.toStringBuffer)
    try {
      state.evalSolution()
    } catch {
      case ContestException(_, Some(p)) =>
        println("Final state:")
        println(state.toStringBuffer.toString().trim)
        val (x, y) = whereBreaks
        assertResult(IPoint(x, y))(p)
      case z : ContestException =>
        println("Final state:")
        println(state.toStringBuffer.toString().trim)
    }
  }


  /*-----------------------------------------------------*/
  /*          Positive tests of the checker              */
  /*-----------------------------------------------------*/

  // Room 0
  s"An evaluator for task $room0" should "work well:" in {}
  checkSolutionProperty(room0, "with-coffee")
  checkSolutionProperty(room0, "with-drill")
  checkSolutionProperty(room0, "with-drill2")
  checkSolutionProperty(room0, "with-teleport")
  checkSolutionProperty(room0, "with-friend")
  checkSolutionProperty(room0, "with-friend2")
  checkSolutionProperty(room0, "with-batteries")
  checkSolutionProperty(room0, "with-friend-batteries")


  def checkSolutionProperty(taskFile: String, solutionFile: String) {
    it should s"satisfy property $solutionFile for task $taskFile" in {
      val testName = s"Test: $taskFile-$solutionFile"
      val (_, state) = createContestState(taskFile)(taskFile, solutionFile)
      val solutionPath = getSolutionPath(taskFile, solutionFile + solutionSuffix)
      val output = FileUtil.readFromFileWithNewLines(solutionPath).mkString
      testWithExpected(state, output, testName)
    }
  }

  /*-----------------------------------------------------*/
  /*          Negative tests of the checker              */
  /*-----------------------------------------------------*/

  s"An evaluator for task $room0" should "break when it must" in {}
  checkBadSolution(room0, "with-drill-too-far", (9, 4))
  checkBadSolution(room0, "with-friend-nope", (0, 9))

  def checkBadSolution(taskFile: String, solutionFile: String, whereBreaks: (Int, Int)) {
    it should s"break with exception for solution $solutionFile for task $taskFile" in {
      val (_, state) = createContestState(taskFile)(taskFile, solutionFile)
      val testName = s"Test: $taskFile-$solutionFile"
      testShouldBreak(state, whereBreaks, testName)
    }
  }

}
