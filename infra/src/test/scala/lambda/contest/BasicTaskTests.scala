package lambda.contest

import java.io.File

import lambda.contest.checkers.ContestCheckingUtils._
import lambda.contest.parsers.ContestTaskParser
import lambda.util.FileUtil
import org.scalatest.{FlatSpec, Matchers}

/**
  * @author Ilya Sergey
  */
class BasicTaskTests extends FlatSpec with Matchers {

  val problemsPath: String = "./infra/src/test/resources/simple/"

  val room1 = "room1"
  val room2 = "room2"
  val room3 = "room3"
  val room4 = "room4"
  val room5 = "room5"
  val room6 = "room6"
  val room7 = "room7"
  val room8 = "room8"
  val room9 = "room9"
  val room10 = "room10"

  def checkTaskFile(fileName: String) {
    s"The problem from file $fileName" should "satisfy all validity checks" in {
      val path = s"$problemsPath/$fileName".replace("/", File.separator)

      val fileContents = FileUtil.readFromFile(path).mkString
      val result = ContestTaskParser(fileContents)
      assert(!result.isEmpty)

      checkTaskWellFormed(result.get)
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



