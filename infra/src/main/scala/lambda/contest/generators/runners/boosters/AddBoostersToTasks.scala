package lambda.contest.generators.runners.boosters

import java.io.File

import lambda.contest.{Booster, ContestTask}
import lambda.contest.checkers.ContestTaskUtils
import lambda.contest.checkers.GraderUtils.PROBLEM_DESC_EXT
import lambda.contest.generators.TaskGeneratorUtils
import lambda.contest.generators.TaskGeneratorUtils.generateBoosters
import lambda.contest.parsers.ContestTaskParser
import lambda.geometry.integer.IPoint
import lambda.util.FileUtil

/**
  * @author Ilya Sergey
  */
object AddBoostersToTasks {

  private val rawPath = "./infra/src/main/resources/contest/obstacles_no_boosters"
  private val finalPath = "./infra/src/main/resources/contest/final"

  def main(args: Array[String]): Unit = {
    val mainDir = new File(rawPath)
    assert(mainDir.exists() && mainDir.isDirectory)
    val subdirs = mainDir.listFiles().toList.filter(_.isDirectory)
    for (d <- subdirs) {
      processDir(d)
    }
  }

  private def processDir(d: File) = {
    for {
      dd <- d.listFiles().toList.filter(_.isDirectory)
      f <- dd.listFiles().filter(_.getName.endsWith(PROBLEM_DESC_EXT))
      line = FileUtil.readFromFileWithNewLines(f.getAbsolutePath).trim
      polyRes = ContestTaskParser(line)
      if !polyRes.isEmpty
      task@ContestTask(room, init, obstacles, Nil) = polyRes.get
    } {
      val bst = getBoostersDependingOnPart(task, getPart(dd))
      val newTask = task.copy(boosters = bst)
      assert(ContestTaskUtils.checkTaskWellFormed(newTask))
      val newPath = s"$finalPath/${dd.getName}/${f.getName}}"
      FileUtil.writeToNewFile(newPath, newTask.toString)
      println(s"Processed file ${f.getName}")
    }
  }
  
  def getPart(dd: File) = {
    if (dd.getName.contains("part-1")) 1
    else if (dd.getName.contains("part-2")) 2
    else 3
  }
  
  def getBoostersDependingOnPart(task: ContestTask, part: Int): List[(Booster.Value, IPoint)] = {
    if (part == 1) 
      return generateBoosters(task, portals = false, forks = false)
    if (part == 2) 
      return generateBoosters(task, portals = true, forks = false)
    generateBoosters(task, portals = true, forks = true)
  }


}
