package lambda.contest.blockchain

import java.io.File

import lambda.contest.ContestErrorMessages.BAD_CHAIN_FILE
import lambda.contest.{ContestErrorMessages, ContestException}
import lambda.geometry.integer.IPolygonUtils.parsePoly
import lambda.geometry.integer.{IPoint, IPolygon, IPolygonUtils}
import lambda.util.FileUtil

/**
  * @author Ilya Sergey
  */
object PuzzleCheckingUtils {

  val ratioDiscrepancy = 0.20

  def checkPolyForPuzzle(poly: IPolygon, boxSize: Int, minVertices: Int, maxVertices: Int): Boolean = {
    val (_, (dx, dy)) = poly.boundingBox
    val pixelDiscrepancy = math.floor(boxSize / 10)

    if (dx > boxSize || dy > boxSize) return false
    if (boxSize - dx > pixelDiscrepancy && boxSize - dy > pixelDiscrepancy) return false
    val vs = poly.vertices
    if (vs.size < minVertices || vs.size > maxVertices) return false
    val area = poly.toFPolygon.area
    val boxArea = boxSize * boxSize.toDouble
    val ratio = area / boxArea
    if (ratio < ratioDiscrepancy) return false
    true
  }


  def getPuzzles(lambdaChainPath: String): List[BlockPuzzle] = {
    val puzzleFile = new File(lambdaChainPath)
    if (!puzzleFile.exists || puzzleFile.isDirectory) {
      throw ContestException(BAD_CHAIN_FILE)
    }

    val lines = FileUtil.readFromFile(puzzleFile.toString).filter(_.trim.isEmpty)
    val puzzles = lines
      .map(l => {
        val strings = l.split("#")
        if (strings.length != 3) {
          throw ContestException(BAD_CHAIN_FILE)
        }
        (strings(0), strings(1), strings(2))
      })
      .map{ case (l, is, os) =>
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
    puzzles
  }


}

case class BlockPuzzle(puzzleNum: Int,
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
                       pointsOutside: List[IPoint])
