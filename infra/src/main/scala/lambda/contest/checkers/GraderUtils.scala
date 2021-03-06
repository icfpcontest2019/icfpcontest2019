package lambda.contest.checkers

import java.io.File

import lambda.contest.{ContestException, ContestTask}
import lambda.contest.checkers.TaskCreationUtils.{contestTaskToMatrix, stringsToTaskMatrix}
import lambda.contest.parsers.ContestTaskParser
import lambda.geometry.integer.IPoint
import lambda.util.FileUtil
import lambda.util.FileUtil.readFromFile

/**
  * @author Ilya Sergey
  */
object GraderUtils {

  val OUT_EXTENSION = ".csv"
  val LOCAL_DIR = "."
  val PROBLEM_PREFIX = "prob-"
  val PROBLEM_DESC_EXT = ".desc"
  val PROBLEM_MATRIX_EXT = ".mat"
  val MATRIX_FOLDER = "matrices"
  val SOLUTION_EXT = ".sol"
  val BOOSTERS_EXT = ".buy"

  type TaskDescription = (TaskMatrix, Int, Int, IPoint)

  def getTaskNumbers(path: String): List[Int] = {
    val dir = new File(path)
    if (!dir.isDirectory)
      throw ContestException(s"File ${dir.getAbsolutePath} is not a directory.")
    val nums = for (f <- dir.listFiles();
                    fname = f.getName
                    if fname.startsWith(PROBLEM_PREFIX) && fname.endsWith(PROBLEM_DESC_EXT);
                    tNum = fname.stripPrefix(PROBLEM_PREFIX).stripSuffix(PROBLEM_DESC_EXT).toInt)
      yield tNum
    nums.toList.sorted
  }

  def readOneTask(mainTaskPath: String, solution: Int): Option[TaskDescription] = {
    val mainDir = new File(mainTaskPath)
    if (!mainDir.isDirectory)
      throw ContestException(s"File ${mainDir.getAbsolutePath} is not a directory.")
    // First try to get as a matrix

    val subDirs = mainDir.listFiles().toList.filter(f => f.isDirectory && f.getName != MATRIX_FOLDER)
    val allDirs = mainDir :: subDirs

    getTaskMatrix(allDirs, solution) match {
      case res@Some(_) => res
      case None =>
        // If not, create the matrix on the fly
        for (f <- allDirs.flatMap(_.listFiles().toList);
             fname = f.getName
             if fname.startsWith(PROBLEM_PREFIX) && fname.endsWith(PROBLEM_DESC_EXT);
             tNum = fname.stripPrefix(PROBLEM_PREFIX).stripSuffix(PROBLEM_DESC_EXT).toInt
             if solution == tNum) {
          val line = readFromFile(f.getAbsolutePath).mkString("").trim
          val task: ContestTask = ContestTaskParser(line).get
          val (matrix, dx, dy) = contestTaskToMatrix(task)
          return Some(matrix, dx, dy, task.initPos)
        }
        None
    }
  }


  def getTaskMatrix(allDirs: List[File], num: Int): Option[TaskDescription] = {
    val allMatrixFolders = allDirs
      .flatMap(_.listFiles().toList)
      .filter(f => f.isDirectory && f.getName == MATRIX_FOLDER)
    for (f <- allMatrixFolders.flatMap(_.listFiles().toList.sorted);
         fname = f.getName
         if fname.startsWith(PROBLEM_PREFIX) && fname.endsWith(PROBLEM_MATRIX_EXT);
         fNum = fname.stripPrefix(PROBLEM_PREFIX).stripSuffix(PROBLEM_MATRIX_EXT).toInt
         if num == fNum) {
      val ls = FileUtil.readFromFile(f.getAbsolutePath)
      val td = stringsToTaskMatrix(ls)
      return Some(td)
    }
    None
  }


}
