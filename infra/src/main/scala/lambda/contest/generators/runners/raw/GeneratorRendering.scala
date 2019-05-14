package lambda.contest.generators.runners.raw

import java.awt.event.ActionEvent
import java.awt.{BorderLayout, Color, Graphics}
import java.io.File

import javax.swing.{BoxLayout, JButton, JFrame, JPanel}
import lambda.contest.checkers.GraderUtils
import lambda.contest.generators.GeneratorFileUtil._
import lambda.contest.generators.PolygonToRender
import lambda.geometry.floating.FPolygon
import lambda.geometry.floating.generators.CompositePolygon

/**
  * @author Ilya Sergey
  */
trait GeneratorRendering {

  val rawPath = "./infra/src/main/resources/contest/no_obstacles_no_boosters"

  protected def generateNewPolygon(boxSize: Int = 100, boundOpt: Option[FPolygon]): CompositePolygon

  var pp: PolygonToRender = _

  /**
    * Draw the generator 
    */
  def draw(boxSize: Int): Unit = {

    val pc = generateNewPolygon(boxSize, None)
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
      pp = PolygonToRender(generateNewPolygon(boxSize, None).pol)
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
    
    val rejectButton = new JButton("Try again")
    rejectButton.addActionListener((e: ActionEvent) => {
      genNewPoly(())
    })

    buttons ++ List(rejectButton)
  }

}
