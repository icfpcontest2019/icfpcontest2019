package lambda.util

import java.io._
import java.math.BigInteger
import java.nio.file.Files
import java.security.SecureRandom
import java.text.ParseException

import lambda.util.project.{BadSetupException, ProjectConfig}
import org.apache.commons.io
import lambda.util.project.ProjectConstants._
import lambda.util.project.{BadSetupException, ProjectConfig}

import scala.io.Source
import scala.util.Try

/**
  * @author Ilya Sergey
  */
object BasicUtils {
  private val DEFAULT_CONFIG_FILE = Seq(System.getProperty("user.dir"), "resources", configFile).mkString(File.separator)
  private val random = new SecureRandom()

  val project_dir: String = System.getProperty("user.dir")
  val resource_path: String = project_dir + File.separator + "resources" + File.separator

  private def getTeamNames(teamNames: String): Seq[String] = Source.fromFile(resource_path + teamNames).getLines.toSeq


  def readProjectConfigFile(sRoot: String): ProjectConfig = {
    val cFile = new File(List(sRoot, configFile).mkString(File.separator))
    if (!cFile.exists()) return new ProjectConfig(root = sRoot)

    val map: Map[String, String] = try {
      val lines = Source.fromFile(cFile).getLines
      val kmap = lines.filter(!_.trim().startsWith(commentToken)).
          map(l => {
            val chunks = l.split(":", 2).toSeq.map(_.trim).filter(_.nonEmpty)
            assert(chunks.size == 2)
            (chunks.head, chunks.tail.head)
          }).toMap
      kmap
    } catch {
      case e: Throwable =>
        e.printStackTrace()
        Map.empty
    }

    try {
      val date = if (map.isDefinedAt(stopDate)) {
        try {
          Some(dateFormat.parse(map(stopDate)))
        } catch {
          case pe: ParseException => None
        }
      } else None

      ProjectConfig(
        host = map.getOrElse(hostKey, DEFAULT_HOST),
        port = Integer.parseInt(map.getOrElse(portKey, DEFAULT_PORT.toString)),
        reportMail = map.get(reportKey),
        stopDate = date,
        root = sRoot
      )
    } catch {
      case e: Throwable => e.printStackTrace()
        ProjectConfig(root = sRoot)
    }
  }

  // Effectful stuff
  def generatePassword(): String = new BigInteger(130, random).toString(32)

  def writeToNewFile(fpath: String, text: String): Unit = {
    val file = new File(fpath)
    if (file.exists()) {
      file.delete()
    }
    if (!file.exists()) {
      file.createNewFile()
    }
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(text)
    bw.close()
  }

  def makeCredentialFiles(tDir: File): Unit = {
    val pFile = tDir.getAbsoluteFile + File.separator + passwordFile
    val eFile = tDir.getAbsoluteFile + File.separator + emailsFile
    writeToNewFile(pFile, generatePassword())
    writeToNewFile(eFile, "")
  }

  def createTeamFolders(teamsPath: String, k: (File) => Unit, teamNames: String): Unit = {
    for (team <- getTeamNames(teamNames)) {
      val tDir = new File(teamsPath + File.separator + team)
      val b = tDir.mkdirs()
      assert(b, s"Cannot create a folder for teacm $team in the path $teamsPath")
      k(tDir)
    }
  }

  def copyConfigFile(sRoot: String) = {
    val confFile = new File(sRoot + File.separator + configFile)
    try {
      val initConfFile = new File(DEFAULT_CONFIG_FILE)
      io.FileUtils.copyFile(initConfFile, confFile)
    } catch {
      case e: IOException =>
        e.printStackTrace()
        writeToNewFile(confFile.getAbsolutePath, "")
    }
  }


  def teamFolderExists(sDirPath: String, tName: String): Boolean = {
    val teamDirPath = List(sDirPath, teamsDir, tName).mkString(File.separator)
    val tFile = new File(teamDirPath)
    tFile.exists() && tFile.isDirectory
  }

  def getTeams(sDirPath: String): Seq[String] = {
    val teamDir = new File(List(sDirPath, teamsDir).mkString(File.separator))
    if (!teamDir.exists() || !teamDir.isDirectory) return Seq.empty
    teamDir.listFiles(new FileFilter {
      override def accept(f: File) = f.isDirectory && !f.getName.contains('.')
    }).toSeq.map(_.getName)
  }

  def deleteFolderRecursively(sDir: File): Try[(Int, Int)] = {
    import scalax.file.Path
    val path: Path = Path(sDir)
    Try(path.deleteRecursively(continueOnFailure = false))
  }

  /**
    * Check if string is a valid email
    */
  def isValidEmail(email: String): Boolean =
    """([\w\.-]+)@([\w\.]+)(\w+)""".r.unapplySeq(email).isDefined

  /*
   * Get a password for a team
   */
  def getTeamPassword(sDirPath: String, tName: String): Option[String] = {
    val teamPasswordPath = List(sDirPath, teamsDir, tName, passwordFile).mkString(File.separator)
    val pFile = new File(teamPasswordPath)
    if (!pFile.exists()) {
      println(s"Password file for the team  $tName should exist in the path $teamPasswordPath!")
      None
    } else {
      val lines = Source.fromFile(pFile).getLines.toSeq
      if (lines.isEmpty) {
        println(s"Password file for the team  $tName in path $teamPasswordPath is empty!")
        None
      } else {
        Some(lines.head.trim)
      }
    }
  }

  /*
   * Send feedback to a team
   */
  def getTeamFeedback(sDirPath: String, tName: String): Option[String] = {
    val feedbackPath = List(sDirPath, teamsDir, tName, feedbackFile).mkString(File.separator)
    val pFile = new File(feedbackPath)
    if (!pFile.exists()) {
      println(s"Feedback file for the team  $tName should exist in the path $feedbackPath!")
      None
    } else {
      val lines = Source.fromFile(pFile).getLines.toSeq
      if (lines.isEmpty) {
        println(s"Password file for the team  $tName in path $feedbackPath is empty!")
        None
      } else {
        Some(lines.mkString("\n"))
      }
    }
  }


  /*
   * Get a list of emails for the team
   */
  def getTeamEmails(sDirPath: String, tName: String): Seq[String] = {
    val teamEmailsPath = List(sDirPath, teamsDir, tName, emailsFile).mkString(File.separator)
    val eFile = new File(teamEmailsPath)
    if (!eFile.exists()) {
      println(s"Email file for the team  $tName should exist in the path $teamEmailsPath!")
      return Seq.empty
    }
    val fromFile = Source.fromFile(eFile).getLines
    val lines = fromFile.toIterator.toSeq
    val trs = for (l <- lines; trimmed = l.trim if trimmed.nonEmpty && isValidEmail(trimmed)) yield trimmed
    trs
  }

  def checkAndCreate(sDirPath: String, solDir: String): AnyVal = {
    val dir = new File(List(sDirPath, solDir).mkString(File.separator))
    if (dir.exists() && !dir.isDirectory) {
      dir.delete()
    }
    if (!dir.exists()) {
      dir.mkdir()
    }
  }

  def getLastSolutionFileNumber(dir: File, prefix: String): (Option[String], Int) = {
    if (!dir.isDirectory) {
      throw new BadSetupException(s"$dir should be a directoryr")
    }

    val files = dir.listFiles(new FilenameFilter {
      override def accept(dir: File, name: String) = name.startsWith(prefix)
    }).toSeq

    val fNumbers: Seq[String] = files.map(_.getName.stripPrefix(prefix)).filter(_.trim.nonEmpty)

    if (fNumbers.isEmpty) return (None, 0)

    // This might throw an exception
    val nums = fNumbers.map(Integer.parseInt)
    val name_max = nums.foldLeft((None: Option[String], 0))((res, num) =>
      if (num > res._2) (Some(prefix + num), num) else res)
    name_max
  }

  def dumpIntoFileAtomically(dir: File, prefix: String, content: String, tmpName: String, num: Int) = {
    val tmpPath = dir.getAbsolutePath + File.separator + tmpName
    val tmpFile = new File(tmpPath)
    writeToNewFile(tmpPath, content)
    val realName = prefix + num
    val realFile = new File(dir.getAbsolutePath + File.separator + realName)
    if (realFile.exists()) {
      throw new BadSetupException(s"File $realFile already exists!")
    }

    // Move atomically
    import java.nio.file.StandardCopyOption._
    Files.move(tmpFile.toPath, realFile.toPath, REPLACE_EXISTING, ATOMIC_MOVE)
  }

}
