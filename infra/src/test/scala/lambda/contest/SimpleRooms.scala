package lambda.contest

import java.io.File

/**
  * @author Ilya Sergey
  */
object SimpleRooms {

  val problemsPath: String = "./infra/src/test/resources/simple/"
  val solutionsPath: String = "./infra/src/test/resources/simple/solutions/"

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

  def getRoomPath(fileName: String) = {
    s"$problemsPath/$fileName".replace("/", File.separator)
  }

  def getSolutionPath(fileName: String) = {
    s"$solutionsPath/$fileName".replace("/", File.separator)
  }

}
