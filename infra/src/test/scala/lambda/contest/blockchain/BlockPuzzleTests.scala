package lambda.contest.blockchain

import java.io.File

import lambda.contest.blockchain.PuzzleCheckingUtils.{checkTaskForSpec, getPuzzleSpecs}
import lambda.contest.checkers.GraderUtils._
import lambda.contest.parsers.ContestTaskParser
import lambda.util.FileUtil
import org.scalatest.{FlatSpec, Matchers}

/**
  * @author Ilya Sergey
  */
class BlockPuzzleTests extends FlatSpec with Matchers {

  private val taskPath = "./infra/src/main/resources/blockchain/puzzles"
  private val specPath = "./infra/src/main/resources/blockchain/lambda.chain"

  s"A block spec checker" should s"succeed for all block puzzles" in {}

  private lazy val tasksNums =
    new File(taskPath).listFiles().toList
      .filter(_.getName.endsWith(PROBLEM_DESC_EXT))
      .sortBy(_.getName)
      .map { f =>
        val num = f.getName.stripSuffix(PROBLEM_DESC_EXT).stripPrefix(PROBLEM_PREFIX).toInt
        val text = FileUtil.readFromFile(f.getAbsolutePath).head
        val task = ContestTaskParser(text).get
        (num, task)
      }

  private lazy val specs = getPuzzleSpecs(specPath)

  for ((n, task) <- tasksNums) {

    it should s"pass for task $n [$taskPath]" in {
      val spec = specs(n)
      val res = checkTaskForSpec(task, spec)
      res match {
        case Right(msg) => assert(false, msg)
        case Left(_) => // ok
      }
    }
  }

}
