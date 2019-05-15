package lambda.contest.generators.runners.raw

import java.awt.event.ActionEvent
import java.awt.{BorderLayout, Color, Graphics}
import java.io.File

import javax.swing.{BoxLayout, JButton, JFrame, JPanel}
import lambda.contest.checkers.GraderUtils
import lambda.contest.generators.GeneratorFileUtil.{getNewFilePath, noObstacleExtension, writeRoomToFile}
import lambda.contest.generators.PolygonToRender
import lambda.geometry.GeometryParsers
import lambda.geometry.integer.IPolygon
import lambda.util.FileUtil

import scala.collection.mutable

/**
  * @author Ilya Sergey
  */
object RawCountryMover {

  val queue = new mutable.Queue[(IPolygon, String)]()
  val claimed = new mutable.HashSet[String]()

  var pp: PolygonToRender = _

  private val rawPath = "./infra/src/main/resources/contest/no_obstacles_no_boosters"
  private val countryPath = "./infra/src/main/resources/geoshapes/countries"

  def main(args: Array[String]): Unit = {
    val boxSize = args(0).toInt
    for (z@(poly, file) <- fetchCountries(boxSize)) {
      queue.enqueue(z)
    }
    // for 
    draw(boxSize)
  }

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
        queue.dequeue()

        if (queue.nonEmpty) {
          pp = PolygonToRender(queue.front._1.toFPolygon)
          polygonPanel.paint(polygonPanel.getGraphics)
        } else {
          System.err.println("Ran out of polygons!")
        }
      }
    }

    frame.setLayout(new BoxLayout(frame.getContentPane, BoxLayout.Y_AXIS))
    frame.add(polygonPanel, BorderLayout.NORTH)

    val buttons = addButtons(generateNewPoly, boxSize)
    buttons.foreach(b => frame.add(b, BorderLayout.SOUTH))

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

    val parts = List(
      ("part-1", "Part 1"),
      ("part-2", "Part 2"),
      ("part-3", "Part 3"),
      ("bonus", "Bonus"),
    )

    val buttons = parts.filter { case (dir, caption) =>
      val f = new File(s"$rawPath/$dir/$boxSize-random/")
      f.isDirectory && f.exists()
    }.map { case (dir, caption) =>
      val button = new JButton(caption)
      button.addActionListener((e: ActionEvent) => {
        recordPolygon(s"$rawPath/$dir/$boxSize-random/")
      })
      button
    }

    val rejectButton = new JButton("Skip for now")
    rejectButton.addActionListener((e: ActionEvent) => {
      genNewPoly(())
    })

    buttons ++ List(rejectButton)
  }


  //////////////////////////////////////////////////////////////////////////////////////////

  private def polyParser = new GeometryParsers {
    def apply(s: String) = parseAll(ipoly, s)
  }

  def fetchCountries(size: Int): List[(IPolygon, String)] = {
    val countryFolder = new File(s"$countryPath/$size")
      .listFiles().toList
      .filter(f => f.getName.endsWith(noObstacleExtension))

    (for {
      f <- countryFolder
      line = FileUtil.readFromFileWithNewLines(f.getAbsolutePath).trim
      polyRes = polyParser(line)
      if !polyRes.isEmpty
      poly = polyRes.get
    } yield (poly, f.getName.stripSuffix(noObstacleExtension))).sortBy(_._2)
  }
  
  private val splitToken = " - "
  
  def fetchClaimedCountries(): List[(String, String)] = {
    val countriesFile = new File(s"$rawPath/countries")
    assert(countriesFile.exists() && !countriesFile.isDirectory)
    val lines = FileUtil.readFromFile(countriesFile.getAbsolutePath).filter(_.trim.nonEmpty)
    lines.map(l => {
      val strings = l.split(splitToken)
      (strings(0), strings(1))
    })
    
    
  }

}
