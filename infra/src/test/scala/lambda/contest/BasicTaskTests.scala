package lambda.contest

import java.io.File

import lambda.contest.checkers.ContestCheckingUtils._
import lambda.contest.checkers.TaskCreationUtils._
import lambda.contest.parsers.ContestTaskParser
import lambda.util.FileUtil
import org.scalatest.{FlatSpec, Matchers}

/**
  * @author Ilya Sergey
  */
class BasicTaskTests extends FlatSpec with Matchers {

  import SimpleRooms._

  def checkTaskFile(fileName: String) {
    s"The problem from file $fileName" should "satisfy all validity checks" in {
      val path = getRoomPath(fileName)
      val fileContents = FileUtil.readFromFile(path).mkString
      val result = ContestTaskParser(fileContents)
      assert(!result.isEmpty)

      val task = result.get
      checkTaskWellFormed(task)

      val (matrix, x, y) = contestTaskToMatrix(task)
      printContestMatrixInAscii(matrix, x, y, List(task.initPos))
      println()
    }
  }

  checkTaskFile(room1)
  checkTaskFile(room2)
  checkTaskFile(room3)
  checkTaskFile(room4)
  checkTaskFile(room5)
  checkTaskFile(room6)
  checkTaskFile(room7)
  checkTaskFile(room8)
  checkTaskFile(room9)
  checkTaskFile(room10)

}



