package lambda.contest.generators.runners.browser

import java.awt.event.{ActionEvent, KeyEvent, KeyListener}
import java.awt.{BorderLayout, Graphics}
import java.io.File

import javax.swing.{BoxLayout, JButton, JFrame, JPanel}
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
object TaskBrowser {

  val tasks = new mutable.ListBuffer[(ContestTask, File)]()

  private var rawPath = "./infra/src/main/resources/contest/final"

  var index = 0

  def main(args: Array[String]): Unit = {
    if (args.nonEmpty) rawPath = args(0)

    fillWithFiles()
    draw()
  }

  def draw(): Unit = {

    val polygonPanel = new JPanel {
      override def paint(g: Graphics) = {
        if (tasks.isEmpty) {
          System.err.println("Nothing to paint, sorry!")
        } else {
          val (task, file) = tasks(index)
          TaskRenderingUtils.renderTask(g, task, file)
        }
      }

    }

    val repaint = (_: Unit) =>
      polygonPanel.paint(polygonPanel.getGraphics)

    val frame = new JFrame with KeyListener {
      override def keyTyped(e: KeyEvent): Unit = e.getKeyCode match {
        case KeyEvent.VK_RIGHT => moveToNext((_: Unit) => paint(getGraphics))
        case KeyEvent.VK_LEFT => moveToPrev((_: Unit) => paint(getGraphics))
        case _ =>
      }

      override def keyPressed(e: KeyEvent): Unit = e.getKeyCode match {
        case KeyEvent.VK_RIGHT => moveToNext((_: Unit) => paint(getGraphics))
        case KeyEvent.VK_LEFT => moveToPrev((_: Unit) => paint(getGraphics))
        case _ =>
      }

      override def keyReleased(e: KeyEvent) {}
    }
    
    frame.setLayout(new BoxLayout(frame.getContentPane, BoxLayout.Y_AXIS))
    frame.add(polygonPanel, BorderLayout.NORTH)

    val buttons = createButtons(repaint)
    buttons.foreach(b => frame.add(b, BorderLayout.SOUTH))

    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    val size = if (tasks.nonEmpty) {
      PolygonToRender(tasks(index)._1.room.toFPolygon).frameSize
    } else (500, 500)
    frame.setSize(size._1, size._2)
    frame.setFocusable(true)
    frame.setFocusTraversalKeysEnabled(false)
    frame.addKeyListener(frame)
    polygonPanel.addKeyListener(frame)
    frame.setVisible(true)

  }

  def createButtons(repaint: Unit => Unit): List[JButton] = {

    // Make new obstacle
    val next = nextButton(repaint)
    val prev = previousButton(repaint)
    List(next, prev)

  }

  def moveToNext(repaint: Unit => Unit): Unit = {
    if (tasks.isEmpty) {
      System.err.println("Oops.")
    } else {
      if (index < tasks.length - 1) {
        index = index + 1
      } else {
        index = 0
      }
      repaint(())
    }
  }


  def moveToPrev(repaint: Unit => Unit): Unit = {
    if (tasks.isEmpty) {
      System.err.println("Oops.")
    } else {
      if (index > 0) {
        index = index - 1
      } else {
        index = tasks.length - 1
      }
      repaint(())
    }
  }


  private def nextButton(repaint: Unit => Unit): JButton = {
    val button = new JButton("Next")
    button.addActionListener((e: ActionEvent) => moveToNext(repaint))
    button
  }

  private def previousButton(repaint: Unit => Unit): JButton = {
    val button = new JButton("Previous")
    button.addActionListener((e: ActionEvent) => moveToPrev(repaint))
    button
  }

  private def fillWithFiles(): Unit = {
    val rawDir = new File(rawPath)
    assert(rawDir.exists() && rawDir.isDirectory)

    // Add all unprocessed files to the queue
    rawDir.listFiles().toList
      .filter(_.isDirectory)
      .flatMap(d => d.listFiles())
      .filter(f => {
        val fName = f.getName
        fName.endsWith(PROBLEM_DESC_EXT)
      })
      .sortBy(_.getName)
      .map(f => {
        val content = FileUtil.readFromFileWithNewLines(f.getAbsolutePath).trim
        val task = ContestTaskParser(content).get
        (task, f)
      })
      .foreach(tasks += _)
  }
}
