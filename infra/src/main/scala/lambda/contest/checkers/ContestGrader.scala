package lambda.contest.checkers

import java.io.File

import lambda.contest.ContestConstants.Action
import lambda.contest.ContestErrorMessages._
import lambda.contest.checkers.TaskCreationUtils.{contestTaskToMatrix, stringsToTaskMatrix}
import lambda.contest.parsers.{BoosterBuyingParser, ContestSolutionParser, ContestTaskParser}
import lambda.contest.{Booster, ContestException, ContestTask}
import lambda.geometry.integer.IPoint
import lambda.util.FileUtil
import lambda.util.FileUtil._
import scopt.OptionParser

/**
  * @author Ilya Sergey
  */
trait ContestGrader {

  import GraderUtils._

  type MyConfig <: GraderConfig

  val defaultConfig: MyConfig

  protected def flagParser: OptionParser[MyConfig]

  def handleInput(paramString: Array[String]): Option[MyConfig] = {
    flagParser.parse(paramString, defaultConfig)
  }


  /* --------------------------------------------------------- */
  /*                    Retrieving tasks                       */
  /* --------------------------------------------------------- */

  /*
   * - Room matrix
   * - X/Y bounding box
   * - Initial position
   */
  type TaskDescription = (TaskMatrix, Int, Int, IPoint)
  type Solution = (List[Booster.Value], List[List[Action]])


  /**
    * @param taskPath path for the tasks
    */
  def readTasksNumbers(taskPath: String): List[Int] = {
    val dir = new File(taskPath)
    if (!dir.isDirectory)
      throw ContestException(s"File ${dir.getAbsolutePath} is not a directory.")

    val nums = for (f <- dir.listFiles();
                    fname = f.getName
                    if fname.startsWith(PROBLEM_PREFIX) && fname.endsWith(PROBLEM_DESC_EXT);
                    tNum = fname.stripPrefix(PROBLEM_PREFIX).stripSuffix(PROBLEM_DESC_EXT).toInt)
      yield tNum
    nums.toList.sorted
  }

  def readOneTask(taskPath: String, solution: Int): Option[TaskDescription] = {
    val dir = new File(taskPath)
    if (!dir.isDirectory)
      throw ContestException(s"File ${dir.getAbsolutePath} is not a directory.")
    // First try to get as a matrix
    getTaskMatrix(dir, solution) match {
      case res@Some(_) => res
      case None =>
        // If not, create the matrix on the fly
        for (f <- dir.listFiles();
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


  def getTaskMatrix(dir: File, num: Int): Option[TaskDescription] = {
    dir.listFiles().toList.find(f => f.isDirectory && f.getName == MATRIX_FOLDER) match {
      case None => None
      case Some(mdir) =>
        for (f <- mdir.listFiles();
             fname = f.getName
             if fname.startsWith(PROBLEM_PREFIX) && fname.endsWith(PROBLEM_MATRIX_EXT);
             fNum = fname.stripPrefix(PROBLEM_PREFIX).stripSuffix(PROBLEM_MATRIX_EXT).toInt
             if num == fNum) {
          val ls = FileUtil.readFromFile(f.getAbsolutePath)
          val td = stringsToTaskMatrix(ls)
          return Some(td)
        }
    }

    None
  }

  /**
    * @param taskPath  path for the tasks
    * @param solutions numbers of provided solutions
    * @return a map from task number to the matrix that describes it  
    */
  def readTasks(taskPath: String, solutions: Set[Int]): Map[Int, TaskDescription] = {
    val dir = new File(taskPath)
    if (!dir.isDirectory)
      throw ContestException(s"File ${dir.getAbsolutePath} is not a directory.")

    var m = Map.empty[Int, TaskDescription]

    for (f <- dir.listFiles();
         fname = f.getName
         if fname.startsWith(PROBLEM_PREFIX) && fname.endsWith(PROBLEM_DESC_EXT);
         tNum = fname.stripPrefix(PROBLEM_PREFIX).stripSuffix(PROBLEM_DESC_EXT).toInt
         if solutions.contains(tNum)) {
      val line = readFromFile(f.getAbsolutePath).mkString("").trim
      val task: ContestTask = ContestTaskParser(line).get
      val (matrix, dx, dy) = contestTaskToMatrix(task)
      m = m + (tNum -> (matrix, dx, dy, task.initPos))
    }
    m
  }

  /* --------------------------------------------------------- */
  /*              Reading solutions and boosters               */
  /* --------------------------------------------------------- */

  def isNumericString(s: String): Boolean = s.forall(Character.isDigit)

  private def getBoosters(solutionFolderPath: String, num: Int): List[Booster.Value] = {
    val dir = new File(solutionFolderPath)
    if (!dir.isDirectory)
      throw ContestException(s"File ${dir.getAbsolutePath} is not a directory.")

    val boosterFileName = PROBLEM_PREFIX + intAs3CharString(num) + BOOSTERS_EXT

    val boosterFile = new File(s"$solutionFolderPath/$boosterFileName")
    if (!boosterFile.exists()) return Nil

    val boosterLine = readFromFile(boosterFile.getAbsolutePath).mkString("").trim
    BoosterBuyingParser(boosterLine) match {
      case BoosterBuyingParser.Success(result, next) => result
      case _ =>
        throw ContestException(s"$BAD_BOOSTER_FORMAT: file $boosterFileName")
    }
  }

  def readSolutionsAndBoosters(solutionFolderPath: String): Map[Int, Solution] = {
    val dir = new File(solutionFolderPath)
    if (!dir.isDirectory)
      throw ContestException(s"File ${dir.getAbsolutePath} is not a directory.")

    var solMap = Map.empty[Int, Solution]

    for (f <- dir.listFiles();
         fname = f.getName
         if fname.startsWith(PROBLEM_PREFIX) && fname.endsWith(SOLUTION_EXT);
         stripped = fname.stripPrefix(PROBLEM_PREFIX).stripSuffix(SOLUTION_EXT)
         if isNumericString(stripped)) {

      val solNum = stripped.toInt
      if (solMap.keySet.contains(solNum)) {
        throw ContestException(s"$DUPLICATED_SOLUTION: file $fname")
      }

      // Obtain boosters
      val boosters = getBoosters(solutionFolderPath, solNum)

      // Obtain solutions
      val solLine = readFromFile(f.getAbsolutePath).mkString("").trim
      ContestSolutionParser(solLine) match {
        case ContestSolutionParser.Success(solution, next) =>
          solMap = solMap + (solNum -> (boosters, solution))
        case _ =>
          throw ContestException(s"$BAD_BOOSTER_FORMAT: file $fname")
      }
    }
    solMap
  }

  /* --------------------------------------------------------- */
  /*                        Grade solutions                    */
  /* --------------------------------------------------------- */

  def gradeSolutions(solutionFolder: String,
                     taskPath: String,
                     solutions: Map[Int, Solution],
                     config: GraderConfig = defaultConfig): Map[Int, Either[Int, String]] = {

    var gradeMap = Map.empty[Int, Either[Int, String]]

    for {taskN <- solutions.keys.toList.sorted} {
      val failMsg = s"Failure while checking the task $taskN."
      val t0 = System.currentTimeMillis()
      val task = try {
        readOneTask(taskPath, taskN)
      } catch {
        case e : Throwable => None
      }
      task match {
        case Some((matrix, dx, dy, init)) =>
          val (boosters, moves) = solutions(taskN)
          val state = TaskExecution.createState(matrix, dx, dy, init, moves, boosters)
          val res = try {
            if (config.verbose) {
              print(s"[$solutionFolder] Grading ${intAs3CharString(taskN)} ... ")
            }
            state.evalSolution() match {
              case Some(value) => Left(value)
              case None => Right(failMsg)
            }
          } catch {
            case ContestException(loc, _) =>
              val msg = s"Failure in task $taskN: $loc."
              Right(msg)
            case _: Throwable => Right(failMsg)
          }
          if (config.verbose) {
            val t1 = System.currentTimeMillis()
            res match {
              case Left(value) => println(s"$value (${t1 - t0} ms)")
              case Right(msg) => println(s"$msg (${t1 - t0} ms)")
            }
          }
          gradeMap = gradeMap + (taskN -> res)
        case None =>
          gradeMap = gradeMap + (taskN -> Right(failMsg))
      }
    }
    gradeMap
  }


  /* --------------------------------------------------------- */
  /*                   Grader config                           */
  /* --------------------------------------------------------- */


  def writeGradesToCSV(taskNumbers: List[Int],
                       gradeMap: Map[Int, Either[Int, String]],
                       outPath: String): Unit = {
    val lines = for (i <- taskNumbers) yield {
      val (steps, status) = gradeMap.getOrElse(i, Right("Failed")) match {
        case Left(z) => (z, "Ok")
        case Right(msg) => (0, "Failed")
      }
      s"$i, $steps, $status"
    }
    FileUtil.writeToNewFile(outPath, lines.mkString("\n"))

  }


  /* --------------------------------------------------------- */
  /*                   Grader config                           */
  /* --------------------------------------------------------- */


  abstract class GraderConfig(val verbose: Boolean)

}
