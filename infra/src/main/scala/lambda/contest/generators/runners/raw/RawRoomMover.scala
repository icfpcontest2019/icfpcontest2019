package lambda.contest.generators.runners.raw

import java.io.File

import lambda.contest.checkers.GraderUtils._
import lambda.contest.generators.GeneratorFileUtil._
import lambda.contest.parsers.ContestTaskParser
import lambda.geometry.GeometryParsers
import lambda.geometry.integer.{IPoint, IPolygon}
import lambda.util.FileUtil

/**
  * @author Ilya Sergey
  */
object RawRoomMover {

  val part1Path = "./infra/src/main/resources/contest/no_obstacles_no_boosters/part-1"

  def main(args: Array[String]): Unit = {
    val boxSize = args(0).toInt

    val outFolder = s"$part1Path/${boxSize}-random"

    var i = 11
    for {
      (poly, init, file) <- getRandomPolygonsAndInitPositions(boxSize)
    } {
      printToTaskFile(poly, init, outFolder, i)
      i = i + 1
    }

  }

  private def polyParser = new GeometryParsers {
    def apply(s: String) = parseAll(ipoly, s)
  }

  def getRandomPolygonsAndInitPositions(size: Int): List[(IPolygon, IPoint, File)] = {
    val asIsFolder = new File(getAsIsPath(size)).listFiles().toList.
      filter(_.getName.endsWith(readyRoomExtension))
    val needObs = new File(getNeedObstaclesPath(size)).listFiles().toList.
      filter(_.getName.endsWith(readyRoomExtension))
    for {
      f <- asIsFolder ++ needObs
      line = FileUtil.readFromFileWithNewLines(f.getAbsolutePath).trim
      polyRes = polyParser(line)
      if !polyRes.isEmpty
      poly = polyRes.get
    } yield (poly, poly.randomCellWithin, f)

  }

  def printToTaskFile(poly: IPolygon, initPos: IPoint,
                      outFolder: String, num: Int): Unit = {

    val polyString = poly.vertices.map {
      _.toString
    }.mkString(",")
    val finalString = List(polyString, initPos.toString, " ", "").mkString(polyParser.sepToken)
    assert(!ContestTaskParser(finalString).isEmpty)

    val out = s"$outFolder/prob-${FileUtil.intAs3CharString(num)}$PROBLEM_DESC_EXT"
    FileUtil.writeToNewFile(out, finalString)

  }

}
