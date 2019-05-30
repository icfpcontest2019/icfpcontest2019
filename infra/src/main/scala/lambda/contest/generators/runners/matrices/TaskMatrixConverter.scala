package lambda.contest.generators.runners.matrices

import java.io.File

import lambda.contest.checkers.TaskCreationUtils.taskToMatrixString
import lambda.contest.parsers.ContestTaskParser
import lambda.util.FileUtil

/**
  * @author Ilya Sergey
  */
object TaskMatrixConverter {

  def main(args: Array[String]): Unit = {
    if (args.length < 2) {
      System.err.println("No file for conversion is provided.")
      return
    }

    val inPath = args(0)
    val outPath = args(1)
    
    val inFile = new File(inPath)
    val line = FileUtil.readFromFileWithNewLines(inFile.getAbsolutePath).trim
    val res = ContestTaskParser(line)
    assert(!res.isEmpty)
    val task = res.get
    val t0 = System.currentTimeMillis()
    val mString = taskToMatrixString(task)
    val t1 = System.currentTimeMillis()
    val tsec = (t1 - t0).toDouble/1000
    FileUtil.writeToNewFile(outPath, mString)
    println(s"$outPath written in $tsec sec")
  }
  

}
