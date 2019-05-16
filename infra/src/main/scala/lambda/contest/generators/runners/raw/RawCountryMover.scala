package lambda.contest.generators.runners.raw

import java.awt.event.ActionEvent
import java.awt.{BorderLayout, Color, Graphics}
import java.io.File

import javax.swing.{BoxLayout, JButton, JFrame, JPanel}
import lambda.contest.checkers.GraderUtils
import lambda.contest.generators.TaskGeneratorUtils.{getNewFilePath, noObstacleExtension, writeRoomToFile}
import lambda.contest.generators.PolygonToRender
import lambda.geometry.GeometryParsers
import lambda.geometry.integer.IPolygon
import lambda.util.FileUtil

import scala.collection.mutable

/**
  * @author Ilya Sergey
  */
object RawCountryMover {

  private val queue = new mutable.Queue[(IPolygon, String)]()
  private val claimed = new mutable.HashSet[(String, String)]()

  private var pp: PolygonToRender = _
  private var cName: String = _

  private val rawPath = "./infra/src/main/resources/contest/no_obstacles_no_boosters"
  private val countryPath = "./infra/src/main/resources/geoshapes/countries"
  private val statesPath = "./infra/src/main/resources/geoshapes/usa_states"

  def main(args: Array[String]): Unit = {
    val boxSize = args(0).toInt
    for (z@(poly, file) <- fetchCountries(boxSize)) {
      queue.enqueue(z)
    }
    for (c <- fetchClaimedCountries()) {
      claimed.add(c)
    }
    draw(boxSize)
  }
  
  def nextNonClaimedCountry(): Option[(IPolygon, String)] = {
    val cs = claimed.toList.map(_._2)
    while (queue.nonEmpty && cs.contains(queue.front._2)) {
      queue.dequeue()
    }
    if (queue.isEmpty) None else Some(queue.front)
  }

  def draw(boxSize: Int): Unit = {
    
    val (poly, cn) = nextNonClaimedCountry().get
    pp = PolygonToRender(poly.toFPolygon)
    cName = cn

    val frame = new JFrame()
    val polygonPanel = new JPanel() {
      override def paint(g: Graphics) = {
        pp.fillWhiteBackground(g)
        pp.fillPoly(g, pp.polygon, Color.LIGHT_GRAY)
        val text = cName
        g.setColor(Color.BLACK)
        g.drawChars(text.toCharArray, 0, text.length, 100, 100)
      }
    }

    def moveToNextCountry: Unit => Unit = { _: Unit =>
      if (queue.isEmpty) {
        System.err.println("Ran out of countries!")
      } else {
        queue.dequeue()

        if (queue.nonEmpty) {
          val (poly, cn) = nextNonClaimedCountry().get
          pp = PolygonToRender(poly.toFPolygon)
          cName = cn
          polygonPanel.paint(polygonPanel.getGraphics)
        } else {
          System.err.println("Ran out of countries!")
        }
      }
    }

    frame.setLayout(new BoxLayout(frame.getContentPane, BoxLayout.Y_AXIS))
    frame.add(polygonPanel, BorderLayout.NORTH)

    val buttons = addButtons(moveToNextCountry, boxSize)
    buttons.foreach(b => frame.add(b, BorderLayout.SOUTH))

    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    val size = pp.frameSize
    frame.setSize(size._1, size._2)
    frame.setVisible(true)
    
  }

  private def recordAllClaimedCountries() = {
    val contents = claimed.toList.map{case (f, c) => s"$f$splitToken$c"}.sorted.mkString("\n")
    FileUtil.writeToNewFile(s"$rawPath/countries", contents)
  }

  private def addButtons(genNewPoly: Unit => Unit, boxSize: Int) = {
    def recordCountry(path: String) = {
      val ext = GraderUtils.PROBLEM_DESC_EXT
      getNewFilePath(path, ext) match {
        case Some(newFile) =>
          val poly = pp.polygon.toIPolygon
          val cn = cName
          val fName = new File(newFile).getName
          writeRoomToFile(newFile, poly)
          claimed.add(fName, cName)
          recordAllClaimedCountries
          println(s"Recorded $cn to $newFile, updated claimed countries")
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
      ("genesis", "genesis"),
      ("bonus", "Bonus"),
    )

    val buttons = parts.filter { case (dir, caption) =>
      val f = new File(s"$rawPath/$dir/$boxSize-countries/")
      f.isDirectory && f.exists()
    }.map { case (dir, caption) =>
      val button = new JButton(caption)
      button.addActionListener((e: ActionEvent) => {
        recordCountry(s"$rawPath/$dir/$boxSize-countries/")
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

    val statesFolder = new File(s"$statesPath/$size")
      .listFiles().toList
      .filter(f => f.getName.endsWith(noObstacleExtension))


    (for {
      f <- (countryFolder ++ statesFolder).sortBy(_.getName)
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
