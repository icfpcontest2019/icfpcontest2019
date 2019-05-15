package lambda.contest.generators.runners.raw

import java.io.File

import lambda.contest.ContestTask
import lambda.contest.checkers.{ContestCheckingUtils, GraderUtils}
import lambda.contest.checkers.GraderUtils.PROBLEM_DESC_EXT
import lambda.contest.generators.GeneratorFileUtil.{getAsIsPath, getNeedObstaclesPath, noObstacleExtension, readyRoomExtension}
import lambda.contest.generators.runners.raw.RawRoomMover.polyParser
import lambda.contest.parsers.ContestTaskParser
import lambda.geometry.integer.IPoint
import lambda.util.FileUtil

/**
  * @author Ilya Sergey
  */
object MoveInitPositionsLeft {

  private val rawPath = "./infra/src/main/resources/contest/no_obstacles_no_boosters"

  def main(args: Array[String]): Unit = {
    val mainDir = new File(rawPath)
    assert(mainDir.exists() && mainDir.isDirectory)
    val subdirs = mainDir.listFiles().toList.filter(_.isDirectory)
    for (d <- subdirs) {
      processDir(d)
    }
  }

  private def processDir(d: File) = {
    val tasks = d.listFiles().toList.filter(_.getName.endsWith(PROBLEM_DESC_EXT))
    for {
      f <- tasks
      line = FileUtil.readFromFileWithNewLines(f.getAbsolutePath).trim
      polyRes = ContestTaskParser(line)
      if !polyRes.isEmpty
      task@ContestTask(room, init, Nil, Nil) = polyRes.get
    } {
      val IPoint(minx, _) = room.getMinXY
      val minLeft = room.vertices.filter(_.x == minx).minBy(_.y)
      val newTask = ContestTask(room, minLeft, Nil, Nil)
      
      assert(ContestCheckingUtils.checkTaskWellFormed(newTask))
      
      FileUtil.writeToNewFile(f.getAbsolutePath, newTask.toString)
    }


  }

  
}
