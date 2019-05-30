package lambda.contest.generators.runners.blockchain

import java.io.File

import lambda.contest.blockchain.PuzzleCheckingUtils
import lambda.contest.generators.runners.raw.RawRoomGenerator.generateNewPolygon
import lambda.geometry.integer.IPolygon
import lambda.util.FileUtil._

/**
  * @author Ilya Sergey
  */
object BlockPolygonGenerator {

  val POLY_EXT = ".pol"
  private val epochPath = "./infra/src/main/resources/blockchain/epochs.chain"
  private val outPath = "./infra/src/main/resources/blockchain/polygons"
  
  def main(args: Array[String]): Unit = {
    val alreadyGenerated = new File(outPath).listFiles().toList
      .filter(_.getName.contains(POLY_EXT))
      .map(f => f.getName.stripSuffix(POLY_EXT).toInt)
      .toSet

    for (p <- loadEpochParams.sortBy(_.puzzleNum);
         num = p.puzzleNum
         if !alreadyGenerated.contains(num)) {
      val poly = generatePolygonWithinBox(p.boxSize, p.minVNum, p.maxVNum)
      val path = s"$outPath/${intAs3CharString(num)}$POLY_EXT"
      writeToNewFile(path, poly.toString)
      println(s"Done with polygon ${p.puzzleNum}")
    }
  }

  case class BlockPuzzleParams
  (puzzleNum: Int,
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
   pointsInsideNum: Int,
   pointsOutsideNum: Int,
  )

  def loadEpochParams: List[BlockPuzzleParams] = {
    val lines = readFromFile(epochPath)
    lines.map { l =>
      val chunk = l.split(",").toList
      val ps = chunk.map(_.toInt)
      BlockPuzzleParams(ps.head, ps(1), ps(2), ps(3), ps(4),
        ps(5), ps(6), ps(7), ps(8), ps(9),
        ps(10), ps(11), ps(12))
    }
  }


  /**
    * Try to generate polygons while doesn't satisfy characteristics  
    */
  private def generatePolygonWithinBox(boxSize: Int, minVertices: Int, maxVertices: Int): IPolygon = {


    val pc = generateNewPolygon(boxSize)
    val poly = pc.pol.toIPolygon.shiftToOrigin
    if (!PuzzleCheckingUtils.checkPolyForPuzzle(poly, boxSize, minVertices, maxVertices)) {
      generatePolygonWithinBox(boxSize, minVertices, maxVertices)
    } else {
      poly
    }
  }


}
