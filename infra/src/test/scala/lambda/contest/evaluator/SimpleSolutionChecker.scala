package lambda.contest.evaluator

import lambda.contest.checkers.TorchShape
import lambda.contest.examples.SimpleRooms
import lambda.contest.ContestConstants
import org.scalatest.{FlatSpec, Matchers}

/**
  * @author Ilya Sergey
  */
class SimpleSolutionChecker extends FlatSpec with Matchers
  with SimpleRooms with SolutionCheckerUtils {

  def checkTaskFile(taskFile: String, solutionFile: String,
                    torchShape: TorchShape = ContestConstants.DEFAULT_CONTEST_TORCH) {
    s"The solution evaluator" should s"work on $taskFile" in {
      val (solution, state) = createContestState(taskFile)(solutionFile)
      assertResult(Some(solution.head.length)) {
        val res = state.evalSolution()
        println(state.toStringBuffer)
        res
      }
    }
  }

  checkTaskFile(room1, room1)

  // Uncomment me and check output!

  //  checkTaskFile(room2, room2)

}
