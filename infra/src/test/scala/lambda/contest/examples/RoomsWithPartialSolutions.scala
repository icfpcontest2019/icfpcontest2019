package lambda.contest.examples

import java.io.File

/**
  * @author Ilya Sergey
  */
trait RoomsWithPartialSolutions {

  val problemsPath: String = s"${System.getProperty("user.dir")}/src/test/resources/navigation/"
  val solutionsPath: String = s"${System.getProperty("user.dir")}/src/test/resources/navigation/routes/"

  val room0 = "room0"

  def getTaskPath(fileName: String) = {
    s"$problemsPath/$fileName".replace("/", File.separator)
  }

  def getSolutionPath(names: String*): String = {
    val fileName = names(0)
    val property = names(1)
    s"$solutionsPath/$fileName-$property".replace("/", File.separator)
  }

}
