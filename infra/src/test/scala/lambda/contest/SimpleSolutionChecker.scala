package lambda.contest

import lambda.contest.checkers.TaskCreationUtils._
import lambda.contest.checkers.{TaskExecution, TorchShape}
import lambda.contest.parsers.ContestSolutionParser
import lambda.util.FileUtil
import org.scalatest.{FlatSpec, Matchers}

/**
  * @author Ilya Sergey
  */
class SimpleSolutionChecker extends FlatSpec with Matchers {

  import SimpleRooms._

  def checkTaskFile(fileName: String,
                    torchShape: TorchShape = ContestConstants.DEFAULT_CONTEST_TORCH) {
    s"The solution evaluator" should s"work on $fileName" in {

      val taskText = FileUtil.readFromFile(getRoomPath(fileName)).mkString
      val task = stringToContestTask(taskText)
      val (matrix, mx, my) = contestTaskToMatrix(task)

      val solutionText = FileUtil.readFromFile(getSolutionPath(fileName)).mkString
      val solution = ContestSolutionParser(solutionText).get

      val state = TaskExecution.createState(matrix, mx, my, task.initPos, solution)

      assertResult(Some(solution.head.length)) {
        val res = state.evalSolution(Nil)
        state.printState()
        res
      }
    }
  }

  checkTaskFile(room1)

  // Uncomment me and check output!

  //  checkTaskFile(room2)

}
