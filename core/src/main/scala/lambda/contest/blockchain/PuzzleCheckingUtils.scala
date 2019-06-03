package lambda.contest.blockchain

import java.io.File

import lambda.contest.ContestErrorMessages._
import lambda.contest.checkers.ContestTaskUtils
import lambda.contest.{Booster, ContestException, ContestTask}
import lambda.geometry.integer.IPolygonUtils.parsePoly
import lambda.geometry.integer.{IPoint, IPolygon}
import lambda.util.FileUtil

/**
  * @author Ilya Sergey
  */
object PuzzleCheckingUtils {

  val ratioDiscrepancy = 0.20

  def checkPolyForPuzzle(poly: IPolygon, boxSize: Int, minVertices: Int, maxVertices: Int): Boolean = {
    val (_, (dx, dy)) = poly.boundingBox
    val pixelDiscrepancy = math.floor(boxSize / 10)

    if (dx > boxSize || dy > boxSize) 
      throw ContestException(TASK_TOO_LARGE_ERROR)
    if (boxSize - dx > pixelDiscrepancy && boxSize - dy > pixelDiscrepancy)
      throw ContestException(TASK_TOO_SMALL_ERROR)
    val vs = poly.vertices
    if (vs.size < minVertices || vs.size > maxVertices)
      throw ContestException(TASK_VERTICES_ERROR)
    val area = poly.toFPolygon.area
    val boxArea = boxSize * boxSize.toDouble
    val ratio = area / boxArea
    if (ratio < ratioDiscrepancy)
      throw ContestException(TASK_AREA_ERROR)
    true
  }
  
  def checkPolyForPuzzleSafe(poly: IPolygon, boxSize: Int, minVertices: Int, maxVertices: Int): Boolean = {
    try {
      checkPolyForPuzzle(poly, boxSize, minVertices, maxVertices)
    } catch {
      case ContestException(_, _) => false
    }
  }
  
  def getPuzzleSpecs(lambdaChainPath: String): Map[Int, BlockPuzzle] = {
    val puzzleFile = new File(lambdaChainPath)
    if (!puzzleFile.exists || puzzleFile.isDirectory) {
      throw ContestException(BAD_CHAIN_FILE)
    }

    val lines = FileUtil.readFromFile(puzzleFile.getAbsolutePath).map(_.trim).filter(_.nonEmpty)
    val puzzles = lines.map(parsePuzzleLine)
    puzzles.map(p => (p.num, p)).toMap
  }
  
  def parsePuzzleLine(line: String) : BlockPuzzle = {
    val strings = line.split("#")
    if (strings.length != 3) {
      throw ContestException(BAD_CHAIN_FILE)
    }
    val (l, is, os) = (strings(0), strings(1), strings(2))
    val chunk = l.split(",").toList
    val ps = chunk.map(_.toInt)
    val isRes = parsePoly(is)
    val osRes = parsePoly(os)
    if (isRes.isEmpty || osRes.isEmpty) {
      throw ContestException(BAD_CHAIN_FILE)
    }
    val inPoints = isRes.get.vertices.toList
    val outPoints = osRes.get.vertices.toList
    BlockPuzzle(ps.head, ps(1), ps(2), ps(3), ps(4),
      ps(5), ps(6), ps(7), ps(8), ps(9),
      ps(10), inPoints, outPoints)
  }

  def checkTaskForSpec(task: ContestTask, spec: BlockPuzzle): Either[Unit, String] = {
    try {
      ContestTaskUtils.checkTaskWellFormed(task)
      val ContestTask(room, _, obs, boosters) = task
      
      if (obs.nonEmpty) 
        throw ContestException(HAS_OBSTACLES_ERROR)
      
      // Check room fitting
      checkPolyForPuzzle(room, spec.boxSize, spec.minVNum, spec.maxVNum)
      
      // Check boosters
      checkBoosterNumbers(spec, boosters)
      
      // Check points inside/outside
      checkPoints(room, spec)

      Left(())
    } catch {
      case ContestException(msg, _) => Right(msg)
      case _ : Throwable => Right(MALFORMED_TASK)
    }
  }

  private def checkPoints(room: IPolygon, spec: BlockPuzzle): Unit ={
    val inPoints = spec.pointsInside
    for (p <- inPoints) {
      if (!room.containsCell(p)) {
        throw ContestException(POINTS_NOT_INSIDE_ERROR)
      }
    }
    
    val outPoints = spec.pointsOutside
    for (p <- outPoints) {
      if (room.containsCell(p)) {
        throw ContestException(POINTS_NOT_OUTSIDE_ERROR)
      }
    }

  }


  private def checkBoosterNumbers(spec: BlockPuzzle, boosters: List[(Booster.Value, IPoint)]) = {
    val boostersGrouped = boosters.map(_._1).groupBy(b => b).map { case (b, ls) => (b, ls.size) }
    val table = spec.getBoosterTable
    val c1 = boostersGrouped.forall { case (b, n) => table.getOrElse(b, 0) == n }
    val c2 = table.forall { case (b, n) => boostersGrouped.getOrElse(b, 0) == n }
    if (!(c1 && c2))
      throw ContestException(HAS_OBSTACLES_ERROR)
  }
}

case class BlockPuzzle(num: Int,
                       epoch: Int,
                       boxSize: Int,
                       minVNum: Int,
                       maxVNum: Int,
                       batteriesNum: Int,
                       coffeeNum: Int,
                       drillNum: Int,
                       portalNum: Int,
                       forkNum: Int,
                       callPointNum: Int,
                       pointsInside: List[IPoint],
                       pointsOutside: List[IPoint]) {

  def getBoosterTable: Map[Booster.Value, Int] = {
    import Booster._
    Map(BatteriesBooster -> batteriesNum,
      CoffeeBooster -> coffeeNum,
      DrillBooster -> drillNum,
      TeleBooster -> portalNum,
      CallBooster -> forkNum,
      CallPoint -> callPointNum)
  }
  

}
