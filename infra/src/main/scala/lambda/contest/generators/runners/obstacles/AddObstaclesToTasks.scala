package lambda.contest.generators.runners.obstacles

import java.awt.{Color, Graphics}
import java.io.File

import javax.swing.{JFrame, JPanel}
import lambda.contest.ContestTask
import lambda.contest.checkers.GraderUtils._
import lambda.contest.generators.PolygonToRender
import lambda.contest.generators.runners.TaskRenderingUtils
import lambda.contest.parsers.ContestTaskParser
import lambda.util.FileUtil

import scala.collection.mutable

/**
  * @author Ilya Sergey
  */
object AddObstaclesToTasks {

  val queue = new mutable.Queue[(ContestTask, File)]()

  private var rawPath = "./infra/src/main/resources/contest/no_obstacles_no_boosters"
  private var obstaclesPath = "./infra/src/main/resources/contest/obstacles_no_boosters"

  def main(args: Array[String]): Unit = {
    val folder = args(0)
    // Yeah, it's nice to be on a dark imperative side...
    rawPath = s"$rawPath/$folder"
    obstaclesPath = s"$obstaclesPath/$folder"
    fillQueueWithFiles()
    draw()
  }

  private def fillQueueWithFiles(): Unit = {
    val rawDir = new File(rawPath)
    assert(rawDir.exists() && rawDir.isDirectory())

    val outDir = new File(obstaclesPath)
    val alreadyProcessed = outDir.listFiles.toList
      .map(_.getName)
      .filter(_.endsWith(PROBLEM_DESC_EXT))
      .toSet

    // Add all unprocessed files to the queue
    rawDir.listFiles().toList
      .filter(_.isDirectory)
      .flatMap(d => d.listFiles())
      .filter(f => {
        val fName = f.getName
        fName.endsWith(PROBLEM_DESC_EXT) &&
        !alreadyProcessed.contains(fName)
      })
      .sortBy(_.getName)
      .map(f => {
        val content = FileUtil.readFromFileWithNewLines(f.getAbsolutePath).trim
        val task = ContestTaskParser(content).get
        (task, f)
      })
      .foreach(queue.enqueue(_))
  }
  
  def draw(): Unit = {
    val frame = new JFrame()
    val polygonPanel = new JPanel() {
      override def paint(g: Graphics) = {
        if (queue.isEmpty) {
          System.err.println("Nothing to paint, sorry!")
        } else {
          val (task, file) = queue.front
          TaskRenderingUtils.renderTask(g, task, file)
        }
      }
    }
  }


}
