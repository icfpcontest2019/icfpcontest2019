package lambda.contest.generators

import java.io.File

import lambda.contest.ContestTask
import lambda.contest.checkers.ContestTaskUtils
import lambda.contest.checkers.ContestTaskUtils.findRandomBox
import lambda.contest.checkers.GraderUtils._
import lambda.contest.parsers.ContestTaskParser
import lambda.geometry.integer.IPolygon
import lambda.util.FileUtil

/**
  * @author Ilya Sergey
  */
object TaskGeneratorUtils {

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

  /**
    * Get generator for the box size 
    */
  def getSuitableGenerator(boxSize: Int = 200, boundOpt: Option[IPolygon]): ContestPolygonGenerator = {
    val size = boundOpt match {
      case Some(poly) =>
        val (dx, dy) = poly.dimensions
        math.min(math.min(dx, dy), boxSize)
      case None => boxSize
    }
    if (size < 10) return ContestGenerators.obstacles_5x5(boundOpt)
    if (size < 20) return ContestGenerators.obstacles_10x10(boundOpt)
    if (size < 30) return ContestGenerators.obstacles_20x20(boundOpt)
    if (size < 50) return ContestGenerators.obstacles_30x30(boundOpt)
    if (size < 150) return ContestGenerators.obstacles_50x50(boundOpt)
    ContestGenerators.largeRoomGenerator(boxSize, boundOpt)
  }

  /**
    * Creates a new obstacle
    */
  def generateNewObstacle(task: ContestTask): Either[ContestTask, String] = {
    findRandomBox(task) match {
      case Some((pt, poly)) =>
        val box =  poly.shiftToOrigin + pt
        val (dx, dy) = box.dimensions
        val numGen = math.min(100, math.min(dx, dy)) 
        val generator = getSuitableGenerator(100, Some(box))
        generator.generate(numGen).sample match {
          case Some(res) =>
            val obs = res.pol.toIPolygon.shiftToOrigin
            assert(obs.isWellFormed && obs.isRectilinear)
            val newTask = task.copy(obstacles = obs :: task.obstacles)
            assert(ContestTaskUtils.checkTaskWellFormed(newTask))
            Left(newTask)
          case None => Right("Couldn't generate an obstacle.")
        }
      case None => Right("Couldn't find the box large enough. :(")
    }
    
  
  }






}
