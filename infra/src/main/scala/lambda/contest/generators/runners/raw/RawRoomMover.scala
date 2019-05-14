package lambda.contest.generators.runners.raw

import java.awt.event.ActionEvent
import java.awt.{BorderLayout, Color, Graphics}
import java.io.File

import javax.swing.{BoxLayout, JButton, JFrame, JPanel}
import lambda.contest.checkers.GraderUtils
import lambda.contest.checkers.GraderUtils._
import lambda.contest.generators.GeneratorFileUtil._
import lambda.contest.generators.PolygonToRender
import lambda.contest.parsers.ContestTaskParser
import lambda.geometry.GeometryParsers
import lambda.geometry.integer.{IPoint, IPolygon}
import lambda.util.FileUtil

import scala.collection.mutable

/**
  * @author Ilya Sergey
  */
object RawRoomMover {

  val queue = new mutable.Queue[(IPolygon, File)]()
  var pp: PolygonToRender = _


  def main(args: Array[String]): Unit = {
    val boxSize = args(0).toInt
    for {
      z@(poly, file) <- getRandomPolygonsAndInitPositions(boxSize)
    } {
      queue.enqueue(z)
    }
    
    draw(boxSize)

  }

  
  private val rawPath = "./infra/src/main/resources/contest/no_obstacles_no_boosters"

  def draw(boxSize: Int): Unit = {

    pp = PolygonToRender(queue.front._1.toFPolygon)

    val frame = new JFrame()
    val polygonPanel = new JPanel() {
      override def paint(g: Graphics) = {
        pp.fillWhiteBackground(g)
        pp.fillPoly(g, pp.polygon, Color.LIGHT_GRAY)
      }
    }

    def generateNewPoly: Unit => Unit = { _: Unit =>
      if (queue.isEmpty) {
        System.err.println("Ran out of polygons!")
      } else {
        val (_, f) = queue.dequeue()
        f.delete()

        if (queue.nonEmpty) {
          pp = PolygonToRender(queue.front._1.toFPolygon)
          polygonPanel.paint(polygonPanel.getGraphics)
        } else {
          System.err.println("Ran out of polygons!")
        }
      }
    }

    val (button1, button2, button3, reject) =
      addButtons(generateNewPoly, boxSize)

    frame.setLayout(new BoxLayout(frame.getContentPane, BoxLayout.Y_AXIS))
    frame.add(polygonPanel, BorderLayout.NORTH)

    frame.add(button1, BorderLayout.SOUTH)
    frame.add(button2, BorderLayout.SOUTH)
    frame.add(button3, BorderLayout.SOUTH)
    frame.add(reject, BorderLayout.SOUTH)

    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    val size = pp.frameSize
    frame.setSize(size._1, size._2)
    frame.setVisible(true)
  }

  def addButtons(genNewPoly: Unit => Unit, boxSize: Int) = {
    def recordPolygon(path: String) = {
      val ext = GraderUtils.PROBLEM_DESC_EXT
      getNewFilePath(path, ext) match {
        case Some(newFile) =>
          val poly = pp.polygon.toIPolygon
          writeRoomToFile(newFile, poly)
          println(s"Recorded polygon to $newFile")
          println()
          genNewPoly(())
        case None =>
          System.err.println(s"Cannot write a new file to the path $path.")
      }
    }

    val partOneButton = new JButton("Part 1")
    val partOneFolder = s"$rawPath/part-1/$boxSize-random/"
    partOneButton.addActionListener((e: ActionEvent) => {
      recordPolygon(partOneFolder)
    })

    val partTwoButton = new JButton("Part 2")
    val partTwoFolder = s"$rawPath/part-2/$boxSize-random/"
    partTwoButton.addActionListener((e: ActionEvent) => {
      recordPolygon(partTwoFolder)
    })

    val partThreeButton = new JButton("Part 3")
    val partThreeFolder = s"$rawPath/part-3/$boxSize-random/"
    partThreeButton.addActionListener((e: ActionEvent) => {
      recordPolygon(partThreeFolder)
    })

    val rejectButton = new JButton("Try again")
    rejectButton.addActionListener((e: ActionEvent) => {
      genNewPoly(())
    })


    (partOneButton, partTwoButton, partThreeButton, rejectButton)
  }

  
  //////////////////////////////////////////////////////////////////////////////////////////
  
  private def polyParser = new GeometryParsers {
    def apply(s: String) = parseAll(ipoly, s)
  }

  def getRandomPolygonsAndInitPositions(size: Int): List[(IPolygon, File)] = {
    val asIsFolder = new File(getAsIsPath(size)).listFiles().toList.
      filter(_.getName.endsWith(readyRoomExtension))
    val needObs = new File(getNeedObstaclesPath(size)).listFiles().toList.
      filter(_.getName.endsWith(noObstacleExtension))
    println()
    for {
      f <- asIsFolder ++ needObs
      line = FileUtil.readFromFileWithNewLines(f.getAbsolutePath).trim
      polyRes = polyParser(line)
      if !polyRes.isEmpty
      poly = polyRes.get
    } yield (poly, f)

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

