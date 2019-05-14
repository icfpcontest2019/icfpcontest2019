package lambda.contest.format

import java.io.File

import lambda.contest.ContestConstants
import lambda.contest.checkers.ContestCheckingUtils._
import lambda.contest.checkers.{GraderUtils, TaskCreationUtils}
import lambda.contest.parsers.ContestTaskParser
import lambda.util.FileUtil
import org.scalatest.{FlatSpec, Matchers}

/**
  * @author Ilya Sergey
  */
class TaskConsistencyTests extends FlatSpec with Matchers {

  "All used letters" should "be distinct" in {
    val letters = ContestConstants.CONTEST_LETTERS.sorted
    val d = letters.distinct
    assertResult(letters)(d)
  }
  
  val rawPath = "./src/main/resources/contest/no_obstacles_no_boosters"
  
  def checkRawTasksInFolder(folder: String): Unit = {
    s"A consistency check for $folder" should "succeed" in {}  
      val dir = new File(s"$rawPath/$folder/")
      assert(dir.isDirectory)
      for (f <- dir.listFiles() if f.getName.endsWith(GraderUtils.PROBLEM_DESC_EXT)) {
        val path = f.getAbsolutePath
        val contents = FileUtil.readFromFileWithNewLines(path).trim
        val res = ContestTaskParser(contents)
        it should s"pass for ${f.getName}" in {
          assert(!res.isEmpty)
          val task = res.get
          // val (matrix, _, _) = TaskCreationUtils.contestTaskToMatrix(task)
          assert(checkTaskWellFormed(task))
        }
    }
  }

  checkRawTasksInFolder("genesis")
  
  checkRawTasksInFolder("part-1/10-simple")
  checkRawTasksInFolder("part-1/30-random")
  checkRawTasksInFolder("part-1/50-random")
  checkRawTasksInFolder("part-1/100-random")
  checkRawTasksInFolder("part-1/100-countries")
  checkRawTasksInFolder("part-1/200-random")
  checkRawTasksInFolder("part-1/200-countries")
  
  checkRawTasksInFolder("part-2/100-random")
  checkRawTasksInFolder("part-2/100-countries")
  checkRawTasksInFolder("part-2/200-random")
  checkRawTasksInFolder("part-2/200-countries")
  checkRawTasksInFolder("part-2/400-random")
  checkRawTasksInFolder("part-2/400-countries")
  
  checkRawTasksInFolder("part-3/100-random")
  checkRawTasksInFolder("part-3/100-countries")
  checkRawTasksInFolder("part-3/200-random")
  checkRawTasksInFolder("part-3/200-countries")
  checkRawTasksInFolder("part-3/400-random")
  checkRawTasksInFolder("part-3/400-countries")

  checkRawTasksInFolder("bonus/400-random")
  checkRawTasksInFolder("bonus/400-countries")
  checkRawTasksInFolder("bonus/600-random")
  
}
