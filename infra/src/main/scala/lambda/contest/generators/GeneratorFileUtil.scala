package lambda.contest.generators

import java.io.File

import lambda.contest.ContestTask
import lambda.contest.checkers.GraderUtils
import lambda.contest.checkers.GraderUtils._
import lambda.contest.parsers.ContestTaskParser
import lambda.geometry.integer.IPolygon
import lambda.util.FileUtil
import org.apache.commons.io.FilenameUtils

/**
  * @author Ilya Sergey
  */
object GeneratorFileUtil {

  private val candidatesPath = "./infra/src/main/resources/candidates/raw/"
  private val asIs = "as-is"
  private val needObstacles = "need-obstacles"

  val readyRoomExtension = ".rroom"
  val noObstacleExtension = ".nobs"
  val positionExtension = ".pos"

  def getCandidatesPath(boxSize: Int) = {
    s"$candidatesPath/$boxSize/".replace("/", File.separator)
  }

  def getAsIsPath(boxSize: Int): String = {
    s"${getCandidatesPath(boxSize)}/$asIs/".replace("/", File.separator)
  }

  def getNeedObstaclesPath(boxSize: Int): String = {
    s"${getCandidatesPath(boxSize)}/$needObstacles/".replace("/", File.separator)
  }

  def getNewFilePath(path: String, ext: String, startNum: Int = 1): Option[String] = {
    val dir = new File(path)
    if (!dir.exists()) {
      dir.mkdirs()
    } else {
      assert(dir.isDirectory)
    }
    val posFile = dir.listFiles().find(f => f.getName.endsWith(positionExtension)).get.getName.stripSuffix(positionExtension)
    val List(startStr, numStr) = posFile.split("-").toList
    val startNum = startStr.toInt
    val totalNum = numStr.toInt


    val fileNumbers: List[Int] =
      dir.listFiles().toList
        .filter(_.getName.endsWith(PROBLEM_DESC_EXT))
        .map(_.getName.stripSuffix(PROBLEM_DESC_EXT).stripPrefix("prob-").toInt)
    val maxNum = if (fileNumbers.isEmpty) startNum else fileNumbers.max + 1
    if (maxNum >= startNum + totalNum) return None


    val newName = s"prob-${FileUtil.intAs3CharString(maxNum)}$ext"
    Some(s"$path/$newName".replace("/", File.separator))

  }

  def writeRoomToFile(newFile: String, poly: IPolygon) = {
    val goodPoly = poly.shiftToOrigin
    val task = ContestTask(goodPoly, goodPoly.randomCellWithin, Nil, Nil)
    assert(!ContestTaskParser(task.toString).isEmpty)
    FileUtil.writeToNewFile(newFile, task.toString)
  }





}
