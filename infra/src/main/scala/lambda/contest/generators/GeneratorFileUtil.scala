package lambda.contest.generators

import java.io.File

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

  def getCandidatesPath(boxSize: Int) = {
    s"$candidatesPath/$boxSize/".replace("/", File.separator)
  }

  def getAsIsPath(boxSize: Int): String = {
    s"${getCandidatesPath(boxSize)}/$asIs/".replace("/", File.separator)
  }

  def getNeedObstaclesPath(boxSize: Int): String = {
    s"${getCandidatesPath(boxSize)}/$needObstacles/".replace("/", File.separator)
  }

  def getNewFilePath(path: String, ext: String) = {
    val file = new File(path)
    if (!file.exists()) {
      file.mkdirs()
    } else {
      assert(file.isDirectory)
    }
    val fileNumbers: List[Int] = 
      file.listFiles().toList.map(f => FilenameUtils.removeExtension(f.getName).toInt)
    val maxNum = if (fileNumbers.isEmpty) 0 else fileNumbers.max + 1
    val newName = "%03d".format(maxNum) + ext
    s"$path/$newName".replace("/", File.separator)

  }


}
