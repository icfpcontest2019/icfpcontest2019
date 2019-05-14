package lambda.contest.generators.runners.raw

import java.io.File

import lambda.contest.ContestTask
import lambda.contest.parsers.ContestTaskParser
import lambda.util.FileUtil

/**
  * @author Ilya Sergey
  */
object RawRoomShifter {
  def main(args: Array[String]): Unit = {
    val path = "./infra/src/main/resources/contest/no_obstacles_no_boosters/part-1/30-random/"
    val dir = new File(path)
    for (f <- dir.listFiles() if f.getName.endsWith(".desc")) {
      val fPath = f.getAbsolutePath
      val task@ContestTask(room, pos, obs, boost) = ContestTaskParser(FileUtil.readFromFileWithNewLines(fPath).trim).get
      val origin = room.getMinXY
      val newRoom = room.shiftToOrigin
      val newPos = pos - origin
      val newTask = ContestTask(newRoom, newPos, obs, boost)
      val line = newTask.toString
      f.delete()
      FileUtil.writeToNewFile(fPath, line)
    }
  }
}
