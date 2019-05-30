package lambda.contest.generators.runners.raw

import java.awt.event.ActionEvent
import java.awt.{BorderLayout, Color, Graphics}
import java.io.File

import javax.swing.{BoxLayout, JButton, JFrame, JPanel}
import lambda.contest.checkers.GraderUtils
import lambda.contest.generators.TaskGeneratorUtils.{getNewFilePath, writeRoomToFile}
import lambda.contest.generators.{PolygonToRender, TaskGeneratorUtils}
import lambda.geometry.floating.FPolygonUtils
import lambda.geometry.floating.generators.CompositePolygon

import scala.util.Random

/**
  * Generating rectilinear polygons
  *
  * Input parameters:
  *
  * boxSize - integer box size
  * path - a path to generate rooms to
  *
  * How to use
  *
  * Rune with arguments, e.g.,
  *
  * 100 my_folder
  *
  * The script will then look for folders in my_folder, 
  * such those they contain subfolders named 100-random (<boxSize>-random) in general. 
  * Each such folder F must also contain a file X-Y.pos, where X is a number with the starting position for numbering
  * and Y is the maximal number of primitives to be generated. 
  *
  * The script will display a UI with all folders F satisfying this condition, to choose which one to generate 
  * obstacles to.
  *
  * @author Ilya Sergey
  */

object RawRoomGenerator {

  var pp: PolygonToRender = _
  var path = "./infra/src/main/resources/contest/no_obstacles_no_boosters"
  var gens = 100


  def main(args: Array[String]) {
    if (args.length < 1) {
      println("Bad format, please specify the size of the bounding box!")
      return
    }
    val boxSize = args(0).toInt

    if (args.length >= 2) {
      path = args(1)
    }
    gens = boxSize

    draw(boxSize)
  }

  /**
    * Draw the generator 
    */
  def draw(boxSize: Int): Unit = {

    val pc = generateNewPolygon(boxSize)
    pp = PolygonToRender(pc.pol)

    val frame = new JFrame()
    val polygonPanel = new JPanel() {
      override def paint(g: Graphics) = {
        pp.fillWhiteBackground(g)
        pp.fillPoly(g, pp.polygon, Color.LIGHT_GRAY)
      }
    }

    def generateNewPoly: Unit => Unit = { _: Unit =>
      pp.fillWhiteBackground(polygonPanel.getGraphics)
      val text = "Rendering new polygon"
      polygonPanel.getGraphics.drawChars(text.toCharArray, 0, text.length, 200, 200)
      pp = PolygonToRender(generateNewPolygon(boxSize).pol)
      polygonPanel.paint(polygonPanel.getGraphics)
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

    val dirs = new File(path).listFiles().toList.filter(_.isDirectory).sortBy(_.getName)
    val parts = dirs.map(f => (f.getName, f.getName.capitalize))
    val buttons = parts.filter { case (dir, caption) =>
      val f = new File(s"$path/$dir/$boxSize-random/")
      f.isDirectory && f.exists()
    }.map { case (dir, caption) =>
      val button = new JButton(caption)
      button.addActionListener((e: ActionEvent) => {
        recordPolygon(s"$path/$dir/$boxSize-random/")
      })
      button
    }

    val rejectButton = new JButton("Try again")
    rejectButton.addActionListener((e: ActionEvent) => {
      genNewPoly(())
    })

    buttons ++ List(rejectButton)
  }

  def generateNewPolygon(boxSize: Int = 100): CompositePolygon = {
    val numGen = gens + Random.nextInt(gens)
    val generator = TaskGeneratorUtils.getSuitableGenerator(boxSize, None)
    val pc = generator.generate(numGen).sample.get
    val ipol = pc.pol.toIPolygon.shiftToOrigin
    assert(ipol.isWellFormed && ipol.isRectilinear)

    println()
    val (x, y) = ipol.dimensions
    println(s"Bounding box: $x x $y")
    println(s"${ipol.vertices.size} vertices")

    val area = FPolygonUtils.computeArea(ipol.toFPolygon)
    val ratio = area / (boxSize * boxSize) * 100

    println(s"Area covered: ${ratio.toInt}%")
    println(ipol.vertices.mkString(", "))
    println()

    pc
  }

}




