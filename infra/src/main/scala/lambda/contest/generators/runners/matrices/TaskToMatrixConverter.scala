package lambda.contest.generators.runners.matrices

import java.io.File

import lambda.contest.checkers.GraderUtils.{PROBLEM_DESC_EXT, PROBLEM_MATRIX_EXT}
import lambda.contest.checkers.TaskCreationUtils.{stringsToTaskMatrix, taskToMatrixString}
import lambda.contest.parsers.ContestTaskParser
import lambda.util.FileUtil

/**
  * @author Ilya Sergey
  */
object TaskToMatrixConverter {

  private var inPath = "./infra/src/main/resources/contest/final"
  private var outPath = s"$inPath/matrices"

  def main(args: Array[String]): Unit = {
    if (args.length > 0) {
      inPath = args(0)
      outPath = s"$inPath/matrices"
    }
    
    val mainDir = new File(inPath)
    assert(mainDir.exists() && mainDir.isDirectory)
    val subdirs = mainDir.listFiles().toList.filter(_.isDirectory)
    for (d <- subdirs.sortBy(_.getName)) {
      processDir(d)
    }
  }

  private def processDir(d: File): Unit = {
    for {
      f <- d.listFiles().filter(_.getName.endsWith(PROBLEM_DESC_EXT)).sortBy(_.getName)
      line = FileUtil.readFromFileWithNewLines(f.getAbsolutePath).trim
      res = ContestTaskParser(line)
      if !res.isEmpty
      task = res.get
    } {
      val t0 = System.currentTimeMillis()
      val mString = taskToMatrixString(task)
      val t1 = System.currentTimeMillis()

      val outDir = new File(s"$outPath/${d.getName}")
      outDir.mkdirs()
      val outName = f.getName.stripSuffix(PROBLEM_DESC_EXT) + PROBLEM_MATRIX_EXT
      val outFile = s"${outDir.getAbsolutePath}/$outName"
      FileUtil.writeToNewFile(outFile, mString)

      val t2 = System.currentTimeMillis()
      val ls = FileUtil.readFromFile(outFile)
      val (matrix, dx, dy, initPos) = stringsToTaskMatrix(ls)
      val t3 = System.currentTimeMillis()

      println(s"Written file ${f.getName} in ${t1 - t0} ms, loaded in ${t3 - t2} ms")
    }
  }


}
