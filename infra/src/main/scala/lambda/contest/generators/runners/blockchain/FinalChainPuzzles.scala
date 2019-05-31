package lambda.contest.generators.runners.blockchain

import java.io.File

import lambda.contest.ContestTask
import lambda.contest.blockchain.BlockPuzzle
import lambda.contest.blockchain.PuzzleCheckingUtils._
import lambda.contest.checkers.ContestTaskUtils
import lambda.contest.checkers.ContestTaskUtils.getVacantCellNotTouchingWalls
import lambda.contest.checkers.GraderUtils.{PROBLEM_DESC_EXT, PROBLEM_PREFIX}
import lambda.contest.generators.runners.blockchain.BlockPolygonGenerator.POLY_EXT
import lambda.geometry.integer.IPolygonUtils
import lambda.util.FileUtil
import lambda.util.FileUtil.intAs3CharString

/**
  * @author Ilya Sergey
  */
object FinalChainPuzzles {

  private val specPath = "./infra/src/main/resources/blockchain/lambda.chain"
  private val polyPath = "./infra/src/main/resources/blockchain/polygons"
  private val finalPath = "./infra/src/main/resources/blockchain/puzzles"

  val PUZZLE_PREFIX = "puzzle-"

  def main(args: Array[String]): Unit = {
    val puzzleSpecs = getPuzzleSpecs(specPath)
    new File(polyPath)
      .listFiles()
      .toList
      .sortBy(_.getName)
      .foreach { f =>
        val num = f.getName.stripSuffix(POLY_EXT).toInt
        val polyString = FileUtil.readFromFile(f.getAbsolutePath).head.trim
        val poly = IPolygonUtils.parsePoly(polyString).get
        val rawTask = ContestTask(poly, poly.randomCellWithin, Nil, Nil)
        val spec = puzzleSpecs(num)
        val taskWithBoosters = beefTaskWithBoosters(rawTask, spec)
        val fName = s"$PROBLEM_PREFIX${intAs3CharString(num)}$PROBLEM_DESC_EXT"
        FileUtil.writeToNewFile(s"$finalPath/$fName", taskWithBoosters.toString)
        println(s"Written task $num")
      }
  }

  private def beefTaskWithBoosters(task: ContestTask, spec: BlockPuzzle): ContestTask = {
    var newTask = task
    for {
      (b, n) <- spec.getBoosterTable
      _ <- 1 to n
    } {
      val p = getVacantCellNotTouchingWalls(newTask)
      val bs = newTask.boosters
      newTask = newTask.copy(boosters = (b, p) :: bs)
    }
    assert(ContestTaskUtils.checkTaskWellFormed(newTask))
    newTask
  }

}
