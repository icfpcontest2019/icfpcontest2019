package lambda.contest.generators.runners.blockchain

import java.io.File

import lambda.contest.generators.runners.blockchain.BlockPolygonGenerator.{POLY_EXT, generatePolygonWithinBox, loadEpochParams, outPath}
import lambda.geometry.GeometryParsers
import lambda.geometry.integer.{IPoint, IPolygon}
import lambda.util.FileUtil
import lambda.util.FileUtil.{intAs3CharString, writeToNewFile}

/**
  * @author Ilya Sergey
  */
object PuzzleRandomPointsGenerator {

  val SPEC_EXT = ".spec"

  private val epochPath = "./infra/src/main/resources/blockchain/epochs.chain"
  private val inPath = "./infra/src/main/resources/blockchain/polygons"
  private val outPath = "./infra/src/main/resources/blockchain/specs"


  def main(args: Array[String]): Unit = {
    val alreadyProcessed = new File(outPath).listFiles().toList
      .filter(_.getName.contains(SPEC_EXT))
      .map(f => f.getName.stripSuffix(SPEC_EXT).toInt)
      .toSet

    for (p <- loadEpochParams.sortBy(_.puzzleNum);
         num = p.puzzleNum
         if !alreadyProcessed.contains(num);
         polyFile = new File(s"$inPath/${intAs3CharString(num)}$POLY_EXT")
         if polyFile.exists()
    ) {
      val s = FileUtil.readFromFile(polyFile.getAbsolutePath).head
      val poly = polyParser(s).get
      val (inSquares, outSquares) = generateInOutSquares(poly, p.pointsInsideNum, p.pointsOutsideNum)
      
      // TODO: generate complete puzzle specification
      
      println(s"Done with polygon ${p.puzzleNum}")
    }

  }
  
  private def generateInOutSquares(poly : IPolygon, inNum: Int, outNum: Int): (List[IPoint], List[IPoint]) = {
    import collection.mutable.{HashSet => MSet}
    
    val pointsIn: MSet[IPoint] = MSet.empty 
    val pointsOut: MSet[IPoint] = MSet.empty
    
    while (pointsIn.size < inNum) {
      val p = poly.randomCellWithin
      if (!pointsIn.contains(p)) {
        pointsIn.add(p)
      }
    }

    while (pointsOut.size < inNum) {
      val p = poly.randomCellOutside
      if (!pointsOut.contains(p)) {
        pointsOut.add(p)
      }
    }
    
    (pointsIn.toList, pointsOut.toList)
    
  }

  private def polyParser = new GeometryParsers {
    def apply(s: String) = parseAll(ipoly, s)
  }


}
