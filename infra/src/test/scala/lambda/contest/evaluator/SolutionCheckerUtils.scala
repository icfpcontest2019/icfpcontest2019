package lambda.contest.evaluator

import lambda.contest.checkers.TaskCreationUtils.{contestTaskToMatrix, stringToContestTask}
import lambda.contest.checkers.TaskExecution
import lambda.contest.parsers.ContestSolutionParser
import lambda.util.FileUtil

/**
  * @author Ilya Sergey
  */
trait SolutionCheckerUtils {
  self: {
    def getTaskPath(s: String): String
    def getSolutionPath(s: String*): String
  } =>

  def createContestState(taskFile: String)(solutionParts: String*) = {
    val taskText = FileUtil.readFromFile(getTaskPath(taskFile)).mkString
    val task = stringToContestTask(taskText)
    val (matrix, mx, my) = contestTaskToMatrix(task)

    val solutionText = FileUtil.readFromFile(getSolutionPath(solutionParts : _*)).mkString
    val solution = ContestSolutionParser(solutionText).get
    val state = TaskExecution.createState(matrix, mx, my, task.initPos, solution)
    (solution, state)
  }


}
