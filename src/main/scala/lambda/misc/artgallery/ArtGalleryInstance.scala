package lambda.misc.artgallery

import java.io.{File, FileFilter, IOException}

import lambda.geometry.{Point2D, Polygon}
import lambda.util.{ScenarioMessage, SubmissionType}
import lambda.util.project.{BadSetupException, ScenarioInstance}
import org.apache.commons.io
import lambda.geometry.{Point2D, Polygon}
import lambda.util._
import lambda.util.project.{BadSetupException, ScenarioInstance}

import scala.io.Source

/**
  * @author Ilya Sergey
  */
object ArtGalleryInstance extends ScenarioInstance {
  import lambda.util.BasicUtils._
  import lambda.util.project.ProjectConstants._

  /* Structure of a Scenario instance

  - tasks
    - guards.pol  // file with polygons
    - check.pol   // file with polygon/guards
  - teams
    - [teamname]*
      - password
      - emails
  - guards
    - [teamname]*
      - results.num
      - dump.num
  - check
    - [teamname]*
      - results.num
      - dump.num
    */

  val uploadGuardPath = "uploadGuards"
  val uploadCheckPath = "uploadCheck"
  val scoreboardGuardsPath = "scoreGuards"
  val scoreboardCheckPath = "scoreCheck"
  val guardsDir = "guards"
  val checkDir = "check"

  val guardsTaskFile = "guards.pol"
  val checkTaskFile = "check.pol"

  val team_names = "animals.abc"

  private val DEFAULT_GUARDS_FILE = Seq(System.getProperty("user.dir"), "resources", "artgallery", "tasks", "guards", guardsTaskFile).mkString(File.separator)
  private val DEFAULT_CHECK_FILE = Seq(System.getProperty("user.dir"), "resources", "artgallery", "tasks", "check", checkTaskFile).mkString(File.separator)


  object GuardSubmission extends SubmissionType {
    override def toSolution(content: Seq[String]): ScenarioMessage = GuardsSolution(content)
    override def toWebPath: String = "/" + uploadGuardPath
    override def toDescription: String = "Problem 1: Computing Guards"
    override def toSolutionFolder: String = guardsDir
    override def toScorePath: String = "/" + scoreboardGuardsPath
  }

  object CheckSubmission extends SubmissionType {
    override def toSolution(content: Seq[String]): ScenarioMessage = CheckSolution(content)
    override def toWebPath: String = "/" + uploadCheckPath
    override def toDescription: String = "Problem 1: Checking Guards"
    override def toSolutionFolder: String = checkDir
    override def toScorePath: String = "/" + scoreboardCheckPath
  }

  def createScenarioSpecificTasks(tasksDir: File): Unit = {

    // Copy task files
    val gFile = new File(tasksDir.getAbsolutePath + File.separator + guardsTaskFile)
    try {
      val sgFile = new File(DEFAULT_GUARDS_FILE)
      io.FileUtils.copyFile(sgFile, gFile)
    } catch {
      case e: IOException =>
        e.printStackTrace()
        writeToNewFile(gFile.getAbsolutePath, "")
    }

    val cFile = new File(tasksDir.getAbsolutePath + File.separator + checkTaskFile)
    try {
      val scFile = new File(DEFAULT_CHECK_FILE)
      io.FileUtils.copyFile(scFile, cFile)
    } catch {
      case e: IOException =>
        e.printStackTrace()
        writeToNewFile(cFile.getAbsolutePath, "")
    }
  }

  /**
    * Create per-team result folders
    * @param innerPath
    */
  override def createResultsFolders(innerPath: String): Unit = {
    val gDir = new File(innerPath + File.separator + guardsDir)
    val b1 = gDir.mkdir()
    if (!b1) {
      println(s"Cannot create folder $gDir")
      return
    }

    // Create separate folders for teams in "guards"
    createTeamFolders(gDir.getAbsolutePath + File.separator, _ => (), team_names)

    val cDir = new File(innerPath + File.separator + checkDir)
    val b2 = cDir.mkdir()
    if (!b2) {
      println(s"Cannot create folder $cDir")
      return
    }

    // Create separate folders for teams in "check"
    createTeamFolders(cDir.getAbsolutePath + File.separator, _ => (), team_names)
  }

  def checkProjectStructure(dir: File): (Boolean, String) = {
    val subdirs = dir.listFiles(new FileFilter {
      override def accept(f: File) = f.isDirectory
    })
    val expected = Set(tasksDir, teamsDir, guardsDir, checkDir)
    val present = subdirs.map(_.getName).toSet
    val b1 = expected.subsetOf(present)
    if (!b1) {
      val diff = expected -- present
      return (false, s"The project doesn't have expected subfolders: $diff")
    }

    val tDir = new File(dir.getAbsolutePath + File.separator + tasksDir)
    if (!tDir.exists() || !tDir.isDirectory) {
      return (false, s"$tasksDir should exist and be a directory")
    }

    val taskFiles = tDir.listFiles().map(_.getName)
    if (!taskFiles.contains(guardsTaskFile)) {
      return (false, s"$tasksDir should contain the file $guardsTaskFile")
    }

    if (!taskFiles.contains(checkTaskFile)) {
      return (false, s"$tasksDir should contain the file $checkTaskFile")
    }

    (true, "OK")
  }

  def checkOrCreateSolutionFolders(sDirPath: String): Unit = {
    checkAndCreate(sDirPath, guardsDir)
    checkAndCreate(sDirPath, checkDir)
  }

  ////////////////////////////////////////////////////////////////////////////////////////////
  ///// Guards-related getters
  ////////////////////////////////////////////////////////////////////////////////////////////

  def getGuardsPolygonMap(sDirPath: String): Map[Int, Polygon] = {
    val guardsFile = new File(List(sDirPath, tasksDir, guardsTaskFile).mkString(File.separator))
    if (!guardsFile.exists()) throw BadSetupException("No guards file found!")

    val polMap = try {
      Source.fromFile(guardsFile).getLines.toSeq.map(GuardInputParser(_).get).toMap
    } catch {
      case e: Throwable =>
        val msg = e.getMessage
        throw BadSetupException(s"Cannot parse the input: $msg")
    }
    polMap
  }

  def retrieveInputForGuards(sDirPath: String, pNum: Int): Either[Polygon, String] = {
    val polMap: Map[Int, Polygon] = getGuardsPolygonMap(sDirPath)
    if (polMap.contains(pNum)) Left(polMap(pNum))
    else Right(s"There is no polygon for the numer $pNum. Check your solution file.")
  }

  ////////////////////////////////////////////////////////////////////////////////////////////
  ///// Check-related getters
  ////////////////////////////////////////////////////////////////////////////////////////////


  type Problem = (Polygon, Seq[Point2D])

  def getProblemMap(sDirPath: String): Map[Int, Problem] = {
    val checkFile = new File(List(sDirPath, tasksDir, checkTaskFile).mkString(File.separator))
    if (!checkFile.exists()) throw BadSetupException("No guards file found!")

    val polMap = try {
      Source.fromFile(checkFile).getLines.toSeq.map(CheckInputParser(_).get).toMap
    } catch {
      case e: Throwable =>
        val msg = e.getMessage
        throw BadSetupException(s"Cannot parse the input: $msg")
    }
    polMap
  }

  def retrieveInputForCheck(sDirPath: String, pNum: Int): Either[Problem, String] = {
    val polMap: Map[Int, (Polygon, Seq[Point2D])] = getProblemMap(sDirPath)
    if (polMap.contains(pNum)) Left(polMap(pNum))
    else Right(s"There is no polygon for the numer $pNum. Check your solution file.")
  }
}
