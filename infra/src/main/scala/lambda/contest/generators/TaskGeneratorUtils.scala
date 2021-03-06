package lambda.contest.generators

import java.io.File

import lambda.contest.Booster.{ArmBooster, CallPoint, CallBooster, WheelsBooster, DrillBooster, TeleBooster}
import lambda.contest.{Booster, ContestTask}
import lambda.contest.checkers.ContestTaskUtils
import lambda.contest.checkers.ContestTaskUtils.{findRandomBox, getVacantCellNotTouchingWalls}
import lambda.contest.checkers.GraderUtils._
import lambda.contest.generators.ContestGenerators.isWithinBoxAtOrigin
import lambda.contest.parsers.ContestTaskParser
import lambda.geometry.integer.{IPoint, IPolygon}
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
    val task = ContestTask(goodPoly, goodPoly.leftMostBottomCell, Nil, Nil)
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
        math.min(math.max(dx, dy), boxSize)
      case None => boxSize
    }
    if (size < 8) {
      println("Choosing 5x5 generator")
      return ContestGenerators.obstacles_5x5(boundOpt)
    }
    if (size < 16) {
      println("Choosing 10x10 generator")
      return ContestGenerators.obstacles_10x10(boundOpt)
    }
    //    if (size < 30) {
    //      println("Choosing 20x20 generator")
    //      return ContestGenerators.obstacles_20x20(boundOpt)
    //    }
    //    if (size < 50) {
    //      println("Choosing 30x30 generator")
    //      return ContestGenerators.obstacles_30x30(boundOpt)
    //    }
    //    if (size < 100) {
    //      println("Choosing default generator")
    //      return ContestGenerators.obstacles_50x50(boundOpt)
    //    }
    ContestGenerators.largeRoomGenerator(boxSize, boundOpt)
  }

  /**
    * Creates a new obstacle
    */
  def generateNewObstacle(task: ContestTask): Either[ContestTask, String] = {
    findRandomBox(task) match {
      case Some((pt, box)) =>

        // Check that the box is okay
        assert(ContestTaskUtils.checkTaskWellFormed(task.copy(obstacles = box :: task.obstacles)))

        val (dx, dy) = box.dimensions
        val numGen = math.min(100, math.min(dx, dy))
        val generator = getSuitableGenerator(200, Some(box))
        generator.generate(numGen).sample match {
          case Some(res)
            if isWithinBoxAtOrigin(Some(box))(res.pol) =>
            val obs = res.pol.toIPolygon.shiftToOrigin + pt

            assert(obs.isWellFormed && obs.isRectilinear)
            assert(box.toFPolygon.contains(obs.toFPolygon))

            assert(task.room.containsPolygonProperly(box))

            val newTask = task.copy(obstacles = obs :: task.obstacles)
            assert(ContestTaskUtils.checkTaskWellFormed(newTask))
            Left(newTask)
          case _ => Right("Couldn't generate an obstacle.")
        }
      case None => Right("Couldn't find the box large enough. :(")
    }

  }

  def dimToCategory(dim: Int, superLarge: Boolean = false): Int = {
    if (superLarge) return 6
    if (dim <= 500) 0
    else if (dim <= 1700) 1
    else if (dim <= 5000) 2
    else if (dim <= 11000) 3
    else if (dim <= 42000) 4
    else 5

  }

  val boosterTable: Map[(Booster.Value, Int), Int] =
    Map(
      (ArmBooster, 0) -> 0,
      (ArmBooster, 1) -> 1,
      (ArmBooster, 2) -> 2,
      (ArmBooster, 3) -> 3,
      (ArmBooster, 4) -> 6,
      (ArmBooster, 5) -> 12,
      (ArmBooster, 6) -> 15,

      (WheelsBooster, 0) -> 0,
      (WheelsBooster, 1) -> 2,
      (WheelsBooster, 2) -> 3,
      (WheelsBooster, 3) -> 5,
      (WheelsBooster, 4) -> 10,
      (WheelsBooster, 5) -> 16,
      (WheelsBooster, 6) -> 18,

      (DrillBooster, 0) -> 0,
      (DrillBooster, 1) -> 1,
      (DrillBooster, 2) -> 1,
      (DrillBooster, 3) -> 2,
      (DrillBooster, 4) -> 5,
      (DrillBooster, 5) -> 9,
      (DrillBooster, 6) -> 11,

      (TeleBooster, 0) -> 0,
      (TeleBooster, 1) -> 0,
      (TeleBooster, 2) -> 0,
      (TeleBooster, 3) -> 1,
      (TeleBooster, 4) -> 1,
      (TeleBooster, 5) -> 2,
      (TeleBooster, 6) -> 3,

      (CallBooster, 0) -> 0,
      (CallBooster, 1) -> 0,
      (CallBooster, 2) -> 0,
      (CallBooster, 3) -> 2,
      (CallBooster, 4) -> 3,
      (CallBooster, 5) -> 4,
      (CallBooster, 6) -> 5,

      (CallPoint, 0) -> 0,
      (CallPoint, 1) -> 1,
      (CallPoint, 2) -> 1,
      (CallPoint, 3) -> 3,
      (CallPoint, 4) -> 4,
      (CallPoint, 5) -> 6,
      (CallPoint, 6) -> 6,
    )


  def generateBoosters(task: ContestTask,
                       portals: Boolean = false,
                       forks: Boolean = false,
                       superLarge: Boolean = false): ContestTask = {
    val ContestTask(room, _, _, _) = task
    val (x, y) = room.dimensions
    val dim = x * y

    var newTask = task
    for {
      b <- Booster.values.toList
      n = boosterTable((b, dimToCategory(dim, superLarge)))
      _ <- 1 to n
    } {
      if (b == Booster.TeleBooster && !portals) {}
      else if (b == Booster.CallBooster && !forks) {}
      else {
        val p = getVacantCellNotTouchingWalls(newTask)
        val bs = newTask.boosters
        newTask = newTask.copy(boosters = (b, p) :: bs)
      }
    }
    assert(ContestTaskUtils.checkTaskWellFormed(newTask))
    newTask
  }


}
