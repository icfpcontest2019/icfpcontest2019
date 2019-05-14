package lambda.contest.generators.runners.raw

import java.awt.event.ActionEvent
import java.awt.{BorderLayout, Color, Graphics}

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

    val (button1, button2, button3, rejectButton) =
      addButtons(generateNewPoly, boxSize)

    frame.setLayout(new BoxLayout(frame.getContentPane, BoxLayout.Y_AXIS))
    frame.add(polygonPanel, BorderLayout.NORTH)

    frame.add(button1, BorderLayout.SOUTH)
    frame.add(button2, BorderLayout.SOUTH)
    frame.add(button3, BorderLayout.SOUTH)
    frame.add(rejectButton, BorderLayout.SOUTH)

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

}
