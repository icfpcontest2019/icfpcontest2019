package lambda.contest.generators.runners.matrices

import java.io.File

import lambda.contest.checkers.GraderUtils._
import lambda.contest.checkers.TaskCreationUtils
import lambda.util.FileUtil

/**
  * @author Ilya Sergey
  */
object BatchPrintTaskSizes {

  private val inPath = "./infra/src/main/resources/contest/final"
  private val outPath = "./infra/src/main/resources/contest/final/sizes.csv"

  def main(args: Array[String]): Unit = {

    val mainDir = new File(inPath)
    assert(mainDir.exists() && mainDir.isDirectory)
    val files = mainDir.listFiles().toList
      .filter(_.isDirectory)
      .flatMap(_.listFiles())
      .filter(_.getName == MATRIX_FOLDER)
      .flatMap(_.listFiles())
      .filter(_.getName.endsWith(PROBLEM_MATRIX_EXT))
      .sortBy(_.getName)
    
    val lines = for (f <- files) yield {
      val name = f.getName
      val num = name.stripPrefix(PROBLEM_PREFIX).stripSuffix(PROBLEM_MATRIX_EXT).toInt
      val ls = FileUtil.readFromFile(f.getAbsolutePath)
      val (_, dx, dy, _) = TaskCreationUtils.stringsToTaskMatrix(ls)
      List(num, dx, dy).mkString(",")
    }
    
    FileUtil.writeToNewFile(outPath, lines.mkString("\n"))
  }

}
