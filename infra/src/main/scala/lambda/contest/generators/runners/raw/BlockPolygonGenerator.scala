package lambda.contest.generators.runners.raw

import lambda.contest.generators.runners.raw.RawRoomGenerator.generateNewPolygon
import lambda.geometry.integer.IPolygon
import lambda.util.FileUtil._

/**
  * @author Ilya Sergey
  */
object BlockPolygonGenerator {
  
  private val POLY_EXT = ".pol"
  private val epochPath = "./infra/src/main/resources/blockchain/epochs.chain"
  private val outPath = "./infra/src/main/resources/blockchain/polygons"


  def main(args: Array[String]): Unit = {
    for (p <- loadEpochParams.sortBy(_.puzzleNum)) {
      val num = p.puzzleNum
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
    lines.map{l => 
      val chunk = l.split(",").toList
      val ps = chunk.map(_.toInt)
      BlockPuzzleParams(ps.head, ps(1), ps(2), ps(3), ps(4), 
        ps(5), ps(6), ps(7), ps(8), ps(9), 
        ps(10), ps(11), ps(12))
    }
  }


  val pixelDiscrepancy = 10
  val ratioDiscrepancy = 0.25

  /**
    * Try to generate polygons while doesn't satisfy characteristics  
    */
  private def generatePolygonWithinBox(boxSize: Int, minVertices: Int, maxVertices: Int): IPolygon = {

    // TODO: Use this as validity checking!
    def checkPoly(poly: IPolygon): Boolean = {
      val (_, (dx, dy)) = poly.boundingBox
      if (dx > boxSize || dy > boxSize) return false
      if (boxSize - dx > pixelDiscrepancy || boxSize - dy > pixelDiscrepancy) return false
      val vs = poly.vertices
      if (vs.size < minVertices || vs.size > maxVertices) return false
      val area = poly.toFPolygon.area
      val boxArea = boxSize * boxSize.toDouble
      val ratio = area / boxArea
      if (ratio < ratioDiscrepancy) return false
      true
    }

    val pc = generateNewPolygon(boxSize)
    val poly = pc.pol.toIPolygon.shiftToOrigin
    if (!checkPoly(poly)) {
      generatePolygonWithinBox(boxSize, minVertices, maxVertices)
    } else {
      poly
    }
  }


}
