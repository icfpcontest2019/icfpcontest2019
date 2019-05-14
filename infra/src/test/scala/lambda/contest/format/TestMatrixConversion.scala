package lambda.contest.format

import java.io.File

import lambda.contest.ContestTask
import lambda.contest.checkers.TaskCreationUtils._
import lambda.contest.checkers.TaskMatrix
import lambda.contest.evaluator.DSASolutions
import lambda.contest.examples.RoomsWithPartialSolutions
import lambda.contest.parsers.ContestTaskParser
import lambda.util.FileUtil
import org.scalatest.{FlatSpec, Matchers}

/**
  * @author Ilya Sergey
  */
class TestMatrixConversion extends FlatSpec with Matchers with RoomsWithPartialSolutions {

  "Task to matrix converter" should s"work for simple rooms" in {}

  private def checkTaskConversion(task: ContestTask, room: String) = {
    it should s"work for room $room" in {

      val t0 = System.currentTimeMillis()
      val s = taskToMatrixString(task)

      val t1 = System.currentTimeMillis()
      val tmpFile = new File("_tmp.tmp")
      FileUtil.writeToNewFile(tmpFile.getAbsolutePath, s)

      val ls = FileUtil.readFromFile(tmpFile.getAbsolutePath)
      tmpFile.delete()

      val (matrix, dx, dy, initPos) = stringsToTaskMatrix(ls)
      val t2 = System.currentTimeMillis()

      println(s"Matrix conversion time for $room: " + (t1 - t0) + " ms")
      println(s"Deserialization time for $room: " + (t2 - t1) + "ms")
      println()

      val (matrix1, dx1, dy1) = contestTaskToMatrix(task)
      val c2 = dx == dx1
      val c3 = dy == dy1
      val c4 = initPos == task.initPos
      assert(c2 && c3 && c4)
      assert(equateMatrices(matrix, matrix1, dx, dy))
    }
  }

  private def equateMatrices(matrix: TaskMatrix, matrix1: TaskMatrix, dx: Int, dy: Int): Boolean = {
    for (i <- 0 until dx;
         j <- 0 until dy) {
      val cell = matrix(i)(j)
      val cell1 = matrix1(i)(j)
      if (cell != cell1) return false
    }
    true
  }

  /////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////

  checkTaskConversion(stringToContestTask(FileUtil.readFromFile(getTaskPath(room0)).mkString), room0)

  val DSA = new DSASolutions {}

  private def readTasks: Map[Int, ContestTask] = {
    (1 to 10).foldLeft(Map.empty[Int, ContestTask]) { case (m, i) =>
      val taskLine = FileUtil.readFromFile(s"${DSA.dsaTaskPath}/room$i").head
      val task: ContestTask = ContestTaskParser(taskLine).get
      m + (i -> task)
    }
  }

  for ((k, v) <- readTasks) {
    checkTaskConversion(v, s"room$k")
  }


  /////////////////////////////////////////////////////////


}
