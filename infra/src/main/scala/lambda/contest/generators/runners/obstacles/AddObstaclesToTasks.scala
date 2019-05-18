package lambda.contest.generators.runners.obstacles

import java.awt.event.ActionEvent
import java.awt.{BorderLayout, Graphics}
import java.io.File

import javax.swing.{BoxLayout, JButton, JFrame, JPanel}
import lambda.contest.ContestTask
import lambda.contest.checkers.GraderUtils._
import lambda.contest.generators.runners.TaskRenderingUtils
import lambda.contest.generators.{PolygonToRender, TaskGeneratorUtils}
import lambda.contest.parsers.ContestTaskParser
import lambda.util.FileUtil

import scala.collection.mutable

/**
  * @author Ilya Sergey
  */
object AddObstaclesToTasks {

  val queue = new mutable.Queue[(ContestTask, File)]()
  var iterations = 10
  var initIterations = 50 

  private var rawPath = "./infra/src/main/resources/contest/no_obstacles_no_boosters"
  private var obstaclesPath = "./infra/src/main/resources/contest/obstacles_no_boosters"
  var currentTaskFile: Option[(ContestTask, File)] = None // Oh boy, this is nasty!

  def main(args: Array[String]): Unit = {
    if (args.isEmpty) {
      System.err.println("No folder given.")
      return
    }

    val folder = args(0)
    // Yeah, it's nice to be on a dark imperative side...
    rawPath = s"$rawPath/$folder"
    obstaclesPath = s"$obstaclesPath/$folder"
    fillQueueWithFiles()

    if (args.length > 0) {
      initIterations = args(1).toInt
      iterations = initIterations
    }

    if (queue.nonEmpty) currentTaskFile = Some(queue.dequeue())
    draw()
  }

  def draw(): Unit = {

    val frame = new JFrame()
    val polygonPanel = new JPanel() {
      override def paint(g: Graphics) = {
        if (currentTaskFile.isEmpty) {
          System.err.println("Nothing to paint, sorry!")
        } else {
          val (task, file) = currentTaskFile.get
          TaskRenderingUtils.renderTask(g, task, file)
        }
      }
    }

    frame.setLayout(new BoxLayout(frame.getContentPane, BoxLayout.Y_AXIS))
    frame.add(polygonPanel, BorderLayout.NORTH)

    val buttons = createButtons((_: Unit) =>
      polygonPanel.paint(polygonPanel.getGraphics))
    buttons.foreach(b => frame.add(b, BorderLayout.SOUTH))

    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    val size = if (currentTaskFile.nonEmpty) {
      PolygonToRender(currentTaskFile.get._1.room.toFPolygon).frameSize
    } else (500, 500)
    frame.setSize(size._1, size._2)
    frame.setVisible(true)

  }

  def createButtons(repaint: Unit => Unit): List[JButton] = {

    // Make new obstacle
    val newObstacle = newObstacleButton(repaint)
    val removeObstacle = removeObstacleButton(repaint)
    val record = recordButton(repaint)
    val skip = skipButton(repaint)

    List(newObstacle, removeObstacle, record, skip)

  }

  private def newObstacleButton(repaint: Unit => Unit): JButton = {
    val button = new JButton("Add more obstacles")
    button.addActionListener((e: ActionEvent) => {
      button.setEnabled(false)
      if (currentTaskFile.isEmpty) {
        System.err.println("Cannot add obstacle: no task given!")
      } else {
        var continue = true
        for (_ <- 1 to iterations if continue) {
          val (task, file) = currentTaskFile.get
          println(s"Generating obstacle ${task.obstacles.size + 1} for task ${file.getName}")
          TaskGeneratorUtils.generateNewObstacle(task) match {
            case Left(newTask) =>
              currentTaskFile = Some(newTask, file)
              repaint(())
            case Right(msg) =>
              System.err.println(msg)
              if (msg.endsWith(":(")) {
                continue = false
              }
          }
        }
        if (iterations >= 20) {
          iterations = iterations / 2
        }
        println()
        println(s"Done generating new obstacles!")
      }
      button.setEnabled(true)
    })
    button
  }


  private def removeObstacleButton(repaint: Unit => Unit): JButton = {
    val button = new JButton("Remove last obstacle")
    button.addActionListener((e: ActionEvent) => {
      if (currentTaskFile.isEmpty) {
        System.err.println("Cannot remove obstacle: no task given!")
      } else {
        val (task, file) = currentTaskFile.get
        val obss = task.obstacles
        if (obss.nonEmpty) {
          val newTask = task.copy(obstacles = obss.tail)
          currentTaskFile = Some(newTask, file)
          repaint(())
        } else {
          System.err.println("No more obstacles to remove")
        }
      }
    })
    button
  }

  private def skipButton(repaint: Unit => Unit): JButton = {
    val button = new JButton("Skip this task")
    button.addActionListener((e: ActionEvent) => {
      if (currentTaskFile.isEmpty) {
        System.err.println("Cannot skip.")
      } else {
        if (queue.nonEmpty) {
          iterations = initIterations
          currentTaskFile = Some(queue.dequeue())
          repaint(())
        } else {
          System.err.println("Finished with the current folder.")
        }
      }
    })
    button
  }

  private def recordButton(repaint: Unit => Unit): JButton = {
    val button = new JButton("Good!")
    button.addActionListener((e: ActionEvent) => {
      if (currentTaskFile.isEmpty) {
        System.err.println("Cannot record")
      } else {
        if (queue.nonEmpty) {
          val (task, file) = currentTaskFile.get
          val outFile = s"$obstaclesPath/${file.getName}"
          FileUtil.writeToNewFile(outFile, task.toString)
          println(s"Written result to ${new File(outFile).getAbsolutePath}")
          currentTaskFile = Some(queue.dequeue())
          repaint(())
          iterations = initIterations
        } else {
          System.err.println("Finished with the current folder.")
        }
      }
    })
    button
  }


  private def fillQueueWithFiles(): Unit = {
    val rawDir = new File(rawPath)
    assert(rawDir.exists() && rawDir.isDirectory())

    val outDir = new File(obstaclesPath)
    assert(outDir.exists() && outDir.isDirectory())

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
      .reverse
      .foreach(queue.enqueue(_))
  }
}
