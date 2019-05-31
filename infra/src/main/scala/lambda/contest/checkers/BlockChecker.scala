package lambda.contest.checkers

import java.io.File

import lambda.contest.ContestConstants.Action
import lambda.contest.ContestErrorMessages._
import lambda.contest.blockchain.BlockPuzzle
import lambda.contest.blockchain.PuzzleCheckingUtils._
import lambda.contest.checkers.GraderUtils.{LOCAL_DIR, PROBLEM_DESC_EXT, SOLUTION_EXT, TaskDescription}
import lambda.contest.checkers.MainContestChecker.{SCRIPTNAME, TOOLNAME}
import lambda.contest.checkers.TaskCreationUtils.stringsToTaskMatrix
import lambda.contest.parsers.{ContestSolutionParser, ContestTaskParser}
import lambda.contest.{ContestException, ContestTask}
import lambda.util.FileUtil
import org.apache.commons.text.StringEscapeUtils


/**
  * The checker takes the following arguments
  *
  * - (p) path to .mat-file for the current task
  * - (c) path to file with puzzle specs
  * - (b) number of the block
  * - (s) path to solution folders
  * 
  * Running from main checker
  * 
  * ./checker block -p <current_task.mat> -c <lambda.chain> -b <blockNum> -s <submissionFolder> -o <scores.csv> -v <true>
  *
  * @author Ilya Sergey
  */


object BlockChecker extends ContestGrader {

  type SimpleSolution = List[List[Action]]

  def main(args: Array[String]): Unit = {

    val configOpt = handleInput(args)
    if (configOpt.isEmpty) {
      return
    }

    val config = configOpt.get

    val spec = getPuzzleSpecs(config.specPath)(config.blockNum)
    if (config.verbose) {
      print("Retrieving submissions... ")
    }
    val submissions = retrieveSubmissions(config.submissionFolderPath)
    if (config.verbose) {
      println(s"got ${submissions.size} submissions for block ${config.blockNum}.")
      println()
    }

    // Now grade all this stuff
    import collection.mutable.{Map => MMap}
    val gradeMap: MMap[String, Option[(Int, String)]] = MMap.empty

    for (tid <- submissions.keys.toList.sorted) {
      if (config.verbose) {
        print(s"[${config.submissionFolderPath}] Grading team $tid ... ")
      }
      val t0 = System.currentTimeMillis()
      val task = getTaskDetails(config.taskMatrixPath)
      val res = gradeSubmission(task, spec, submissions(tid))
      gradeMap.put(tid, res)
      val t1 = System.currentTimeMillis()
      if (config.verbose) {
        val delta = (t1 - t0).toDouble / 1000
        println(s"done ($delta sec)")
      }
    }

    writeScoreToCSV(gradeMap.toMap, config.outPath)
    if (config.verbose && gradeMap.nonEmpty) {
      println()
      println(s"Finished. The scores are written into ${config.outPath}")
    }
  }

  type Submission = Either[String, (SimpleSolution, ContestTask)]

  private val GOOD = "GOOD"
  private val BAD = "BAD"

  def writeScoreToCSV(gradeMap: Map[String, Option[(Int, String)]],
                      outPath: String): Unit = {

    val lines = for (team <- gradeMap.keys.toList.sorted) yield {
      val escapedTeamName = StringEscapeUtils.escapeCsv(team)
      gradeMap(team) match {
        case Some((score, res)) => List(escapedTeamName, score.toString, res).mkString(",") 
        case None => List(escapedTeamName, 0, BAD).mkString(",")
      }
    }

    FileUtil.writeToNewFile(outPath, lines.mkString("\n"))
  }


  private def gradeSubmission(blockTask: TaskDescription, blockSpec: BlockPuzzle,
                              submission: Submission): Option[(Int, String)] = {
    submission match {
      case Left(_) => None // Bad submission 
      case Right((moves, newTask)) =>
        // First grade the solution
        try {
          // First check the solution
          val (matrix, dx, dy, init) = blockTask
          val state = TaskExecution.createState(matrix, dx, dy, init, moves, Nil)
          val evalResult = state.evalSolution()
          if (evalResult.isEmpty) return None
          val roundsSpent = evalResult.get

          // Now check the proposal 
          checkTaskForSpec(newTask, blockSpec) match {
            case Left(()) => // OK
              Some((roundsSpent, GOOD))
            case Right(_) =>
              Some((roundsSpent, BAD))
          }
        } catch {
          case z: Throwable => 
            None
        }


    }

  }

  /**
    * Returns a map from team id, to their
    * * current task solution
    * * next task proposal
    */
  private def retrieveSubmissions(submissionPath: String): Map[String, Submission] = {
    val sFolder = new File(submissionPath)
    assert(sFolder.isDirectory)
    val teamFolders = sFolder.listFiles().toList.filter(_.isDirectory)

    import collection.mutable.{Map => MMap}
    val subMap: MMap[String, Submission] = MMap.empty

    for (tdir <- teamFolders) {
      val tid = tdir.getName
      try {
        val files = tdir.listFiles().toList
        // First get the submission file
        val solution = fetchSolutionForTeam(files)
        // Now get the puzzle proposal file
        val task = fetchTaskForTeam(files)
        subMap.put(tid, Right((solution, task)))
      } catch {
        case ContestException(msg, _) => subMap.put(tid, Left(s"Failure: $msg"))
          _: Throwable => subMap.put(tid, Left(s"Failed"))
      }
    }

    subMap.toMap
  }

  private def fetchTaskForTeam(files: List[File]): ContestTask = {
    val taskRes = files.find(f => f.getName.endsWith(PROBLEM_DESC_EXT))
    if (taskRes.isEmpty)
      throw ContestException(NO_PUZZLE_FILE)
    val taskText = FileUtil.readFromFile(taskRes.get.getAbsolutePath).mkString("").trim
    val taskParseResult = ContestTaskParser(taskText)
    if (taskParseResult.isEmpty)
      throw ContestException(MALFORMED_TASK)
    taskParseResult.get
  }

  private def fetchSolutionForTeam(files: List[File]): SimpleSolution = {
    val solRes = files.find(f => f.getName.endsWith(SOLUTION_EXT))
    if (solRes.isEmpty)
      throw ContestException(NO_SOLUTION_FILE)
    val solutionText = FileUtil.readFromFile(solRes.get.getAbsolutePath).mkString("").trim
    val solParseResult = ContestSolutionParser(solutionText)
    if (solParseResult.isEmpty)
      throw ContestException(BAD_SOLUTION_FORMAT)
    solParseResult.get
  }

  private def getTaskDetails(taskMatrixPath: String): TaskDescription = {
    val mFile = new File(taskMatrixPath)
    assert(mFile.exists())
    val ls = FileUtil.readFromFile(mFile.getAbsolutePath)
    stringsToTaskMatrix(ls)
  }


  case class BlockCheckerConfig(submissionFolderPath: String = LOCAL_DIR,
                                blockNum: Int = 1,
                                taskMatrixPath: String = LOCAL_DIR,
                                specPath: String = LOCAL_DIR,
                                outPath: String = s"$LOCAL_DIR/score.csv",
                                override val verbose: Boolean = false)
    extends GraderConfig(verbose)

  type MyConfig = BlockCheckerConfig

  override val defaultConfig = BlockCheckerConfig()
  
  protected val flagParser = new scopt.OptionParser[BlockCheckerConfig](SCRIPTNAME) {
    head(TOOLNAME, MainContestChecker.VERSION_STRING)

    opt[String]('p', "problem")
      .required()
      .valueName("<currentProblemFile>")
      .action { (x, c) =>
        c.copy(taskMatrixPath = x)
      }.text("A path to the task file (as a matrix .mat)")


    opt[Int]('b', "block")
      .required()
      .valueName("<BlockNum>")
      .action { (x, c) =>
        c.copy(blockNum = x)
      }.text("Block number")

    opt[String]('c', "specs")
      .required()
      .valueName("<solutionsPath>")
      .action { (x, c) =>
        c.copy(specPath = x)
      }.text("A path to the file with puzzle specifications")

    opt[String]('s', "submissions")
      .required()
      .valueName("<submissionFolderPath>")
      .action { (x, c) =>
        c.copy(submissionFolderPath = x)
      }.text("A path to the folder with individual team submissions")


    opt[String]('o', "out")
      .required()
      .valueName("<outputFilePath>")
      .action { (x, c) =>
        c.copy(outPath = x)
      }.text("An output file")

    opt[Boolean]('v', "verbose")
      .valueName("<verbose>")
      .action { (x, c) =>
        c.copy(verbose = true)
      }.text("Verbose output")

    help("help").text("prints this usage text")
  }


}
