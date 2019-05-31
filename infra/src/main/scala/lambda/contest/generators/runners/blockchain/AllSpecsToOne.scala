package lambda.contest.generators.runners.blockchain

import java.io.File

import lambda.contest.generators.runners.blockchain.RandomPointsForPuzzles.SPEC_EXT
import lambda.util.FileUtil

/**
  * @author Ilya Sergey
  */
object AllSpecsToOne {

  def main(args: Array[String]): Unit = {
    val outPath = "./infra/src/main/resources/blockchain/specs"
    val content = new File(outPath).listFiles().toList
      .filter(_.getName.endsWith(SPEC_EXT))
      .sortBy(_.getName)
      .map(f => {
        val line = FileUtil.readFromFile(f.getAbsolutePath).head
        // val num = f.getName.stripSuffix(SPEC_EXT).toInt
        line
      })
      .mkString("\n")
    
    val outFile = "./infra/src/main/resources/blockchain/lambda.chain"
    FileUtil.writeToNewFile(outFile, content)
  }

}
