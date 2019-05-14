package lambda.contest.checkers

import java.io.File

import lambda.contest.ContestConstants.Action
import lambda.contest.ContestErrorMessages._
import lambda.contest.checkers.TaskCreationUtils.contestTaskToMatrix
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

  val OUT_EXTENSION = ".csv"
  val LOCAL_DIR = "."
  val PROBLEM_PREFIX = "prob-"
  val PROBLEM_DESC_EXT = ".desc"
  val SOLUTION_EXT = ".sol"
  val BOOSTERS_EXT = ".buy"

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

  def gradeSolutions(tFolder: String,
                     tasks: Map[Int, TaskDescription],
                     solutions: Map[Int, Solution],
                     config: GraderConfig = defaultConfig): Map[Int, Option[Int]] = {

    var gradeMap = Map.empty[Int, Option[Int]]

    for {taskN <- tasks.keys.toList.sorted
         (matrix, dx, dy, init) = tasks(taskN)} {
      if (!solutions.isDefinedAt(taskN)) {
        gradeMap = gradeMap + (taskN -> None)
      } else {
        val (boosters, moves) = solutions(taskN)
        val state = TaskExecution.createState(matrix, dx, dy, init, moves, boosters)
        val res = try {
          if (config.verbose) {
            print(s"[$tFolder] Grading ${intAs3CharString(taskN)} ... ")
          }
          state.evalSolution()
        } catch {
          case _: Throwable => None
        }
        if (config.verbose) {
          res match {
            case Some(value) => println(s"$value")
            case None => println("failed")
          }
        }
        gradeMap = gradeMap + (taskN -> res)
      }
    }
    gradeMap
  }


  /* --------------------------------------------------------- */
  /*                   Grader config                           */
  /* --------------------------------------------------------- */


  def writeGradesToCSV(gradeMap: Map[Int, Option[Int]], outPath: String): Unit = {
    val lines = for (i <- gradeMap.keySet.toList.sorted) yield {

      val res = gradeMap(i)
      val steps = if (res.isDefined) res.get else 0
      val status = if (res.isDefined) "Ok" else "Failed"
      s"$i, $steps, $status"
    }
    FileUtil.writeToNewFile(outPath, lines.mkString("\n"))

  }


  /* --------------------------------------------------------- */
  /*                   Grader config                           */
  /* --------------------------------------------------------- */


  abstract class GraderConfig(val verbose: Boolean)

}
