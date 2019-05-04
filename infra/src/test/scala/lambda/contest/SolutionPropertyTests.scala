package lambda.contest

import lambda.contest.checkers.TaskExecution
import lambda.contest.examples.RoomsWithPartialSolutions
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
    state.evalSolution(Nil)
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
      state.evalSolution(Nil)
    } catch {
      case ContestException(_, p: IPoint) =>
        println("Final state:")
        println(state.toStringBuffer.toString().trim)
        val (x, y) = whereBreaks
        assertResult(IPoint(x, y))(p)
    }
  }


  /*-----------------------------------------------------*/
  /*          Positive tests of the checker              */
  /*-----------------------------------------------------*/

  def checkSolutionProperty(taskFile: String, solutionFile: String) {

    s"The evaluator for task $taskFile" should s"satisfy property $solutionFile" in {
      val testName = s"Test: $taskFile-$solutionFile"
      val (_, state) = createContestState(taskFile)(taskFile, solutionFile)
      val solutionPath = getSolutionPath(taskFile, solutionFile + solutionSuffix)
      val output = FileUtil.readFromFileWithNewLines(solutionPath).mkString
      testWithExpected(state, output, testName)
    }
  }

  checkSolutionProperty(room0, "with-coffee")
  checkSolutionProperty(room0, "with-drill")
  checkSolutionProperty(room0, "with-drill2")


  /*-----------------------------------------------------*/
  /*          Negative tests of the checker              */
  /*-----------------------------------------------------*/

  def checkBadSolution(taskFile: String, solutionFile: String, whereBreaks: (Int, Int)) {
    s"The evaluator for task $taskFile" should s"break with exception for solution $solutionFile" in {
      val (_, state) = createContestState(taskFile)(taskFile, solutionFile)
      val testName = s"Test: $taskFile-$solutionFile"
      testShouldBreak(state, whereBreaks, testName)
    }
  }

  checkBadSolution(room0, "with-drill-too-far", (9, 4))

}
