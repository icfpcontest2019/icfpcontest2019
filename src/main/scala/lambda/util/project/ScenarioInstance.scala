package lambda.util.project

import java.io._
import java.util.Date

import ProjectConstants._
import lambda.util.{BasicUtils, SubmissionType}
import lambda.util.SubmissionType

trait ScenarioInstance {
  import lambda.util.BasicUtils._

  val team_names : String
  def createScenarioSpecificTasks(tasksDir: File)

  val uploadPath = "upload"
  val scoreboardPath = "scores"
  val solutionsDir = "solutions"

  type Problem
  def getProblemMap(sDirPath: String): Map[Int, Problem]

  def createTasksFolder(tasksDirPath: String): Unit = {
    val tasksDir = new File(tasksDirPath)
    if (tasksDir.exists()) {
      println(s"The directory $tasksDirPath already exists")
      return
    }
    tasksDir.mkdir()
    createScenarioSpecificTasks(tasksDir)
  }

  def createResultsFolders(innerPath: String): Unit = {
    val gDir = new File(innerPath + File.separator + solutionsDir)
    val b1 = gDir.mkdir()
    if (!b1) {
      println(s"Cannot create folder $gDir")
      return
    }
    // Create separate folders for teams
    createTeamFolders(gDir.getAbsolutePath + File.separator, _ => (), team_names)
  }

  // Create new scenario instance
  def populateInstance(basepath: String, name: String,
                       deleteOld: Boolean = false, testEmail: Option[String]): String = {
    val instancesDir = new File(basepath)
    if (!instancesDir.exists() || !instancesDir.isDirectory) {
      println("Cannot create a project here!")
      return ""
    }

    val sDir = new File(basepath + File.separator + name)
    if (sDir.exists() && !deleteOld) {
      println("Such scenario project already exists")
      return ""
    } else if (sDir.exists() && deleteOld) {
      deleteFolderRecursively(sDir)
    }

    val b = sDir.mkdir()
    if (!b) {
      println(s"Something is wrong here, cannot make directory $sDir")
      return ""
    }

    val innerPath = sDir.getAbsolutePath + File.separator
    createTasksFolder(innerPath + tasksDir)
    copyConfigFile(innerPath)
    createTeamFolders(innerPath + teamsDir, makeCredentialFiles, team_names)
    createResultsFolders(innerPath)

    sDir.getAbsolutePath
  }

  def checkProjectStructure(dir: File): (Boolean, String)

  def getTeamResultFolder(sDirPath: String, tname: String, st: SubmissionType): Option[File] = {
    val solDir = new File(List(sDirPath, st.toSolutionFolder, tname).mkString(File.separator))
    if (!solDir.exists()) {
      solDir.mkdir()
    }
    if (!solDir.exists() || !solDir.isDirectory) None
    else Some(solDir)
  }

  def sendResult(sDir: String, st: SubmissionType, tname: String, time: Date, body: String) {
    import lambda.util.email.EmailUtils._
    val subj = s"[$tname] Report on ${st.toDescription}"
    val tos = getTeamEmails(sDir, tname)
    val mails = for (tz <- tos) yield Mail(to = tz, subject = subj, message = body)
    mails foreach send
  }

  def writeIntoResultFile(sDirPath: String, tName: String, st: SubmissionType, content: String, prefix: String): Unit = {
    val tfres = getTeamResultFolder(sDirPath, tName, st)
    if (tfres.isEmpty) {
      throw BadSetupException(s"There is no team folder in $sDirPath for the team $tName")
    }
    val dir = tfres.get
    val num = getLastSolutionFileNumber(dir, prefix)._2 + 1
    // Dump and move
    dumpIntoFileAtomically(dir, prefix, content, prefix + tmpSuffix, num)
  }

  // run at strartup
  def checkOrCreateSolutionFolders(sDirPath: String): Unit


  def getLastResultPath(sDirPath: String, tName: String, st: SubmissionType): Option[String] = {
    val tfres = getTeamResultFolder(sDirPath, tName, st)
    if (tfres.isEmpty) {
      return None
    }
    val dir = tfres.get
    val fname = BasicUtils.getLastSolutionFileNumber(dir, resultsPrefix)._1
    if (fname.isEmpty) return None
    val fPath = dir.getAbsolutePath + File.separator + fname.get
    Some(fPath)
  }

}










