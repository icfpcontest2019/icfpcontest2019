package lambda.contest.generators

import java.awt.event.{ActionEvent, ActionListener}
import java.awt.{BorderLayout, Color, Graphics}

import javax.swing.{BoxLayout, JButton, JFrame, JPanel}
import lambda.contest.generators.GeneratorFileUtil.{getAsIsPath, getNeedObstaclesPath, getNewFilePath, noObstacleExtension, readyRoomExtension}
import lambda.geometry.floating.FPolygon
import lambda.geometry.floating.generators.CompositePolygon
import lambda.util.FileUtil

/**
  * @author Ilya Sergey
  */
trait GeneratorRendering {

  protected def generateNewPolygon(boxSize: Int = 100, boundOpt: Option[FPolygon]): CompositePolygon


  var pp : PolygonToRender = _
  
  /**
    * Draw the generator 
    */
  def draw(boxSize: Int, boundOpt: Option[FPolygon]): Unit = {

    val pc = generateNewPolygon(boxSize, boundOpt)
    pp = PolygonToRender(pc.pol)

    val frame = new JFrame()
    val polygonPanel = new JPanel() {
      override def paint(g: Graphics) = {
        val boundPP = PolygonToRender(if (boundOpt.isDefined) boundOpt.get else pc.pol)
        boundPP.fillWhiteBackground(g)
        boundOpt match {
          case Some(x) =>
            boundPP.drawPoly(g, x, Color.BLACK)
          case None =>
        }
        boundPP.fillPoly(g, pp.polygon, Color.LIGHT_GRAY)
      }
    }

    def generateNewPoly: Unit => Unit = { _ : Unit =>
      pp = PolygonToRender(generateNewPolygon(boxSize, boundOpt).pol)
      polygonPanel.paint(polygonPanel.getGraphics)
    }


    val (acceptButton, needObstaclesButton, rejectButton) = addButtons(generateNewPoly, boxSize)

    frame.setLayout(new BoxLayout(frame.getContentPane, BoxLayout.Y_AXIS))
    frame.add(polygonPanel, BorderLayout.NORTH)
    
    frame.add(acceptButton, BorderLayout.SOUTH)
    frame.add(needObstaclesButton, BorderLayout.SOUTH)
    frame.add(rejectButton, BorderLayout.SOUTH)

    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    val size = pp.frameSize
    frame.setSize(size._1, size._2)
    frame.setVisible(true)
  }
  
  def addButtons(genNewPoly: Unit => Unit, boxSize: Int) = {
    def recordPolygon(path: String, ext: String) = {
      val newFile = getNewFilePath(path, ext)
      val poly = pp.polygon.toIPolygon
      FileUtil.writeToNewFile(newFile, poly.toString)
      println(s"Recorded polygon to $newFile")
      println()
      genNewPoly(())
    }

    val acceptButton = new JButton("Awesome!")
    acceptButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        recordPolygon(getAsIsPath(boxSize), readyRoomExtension)
      }
    })
    
    val needObstaclesButton = new JButton("Needs obstacles...")
    needObstaclesButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        recordPolygon(getNeedObstaclesPath(boxSize), noObstacleExtension)
      }
    })

    val rejectButton = new JButton("Try again")
    rejectButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        genNewPoly(())
      }
    })
    
    (acceptButton, needObstaclesButton, rejectButton)
  }
  
}
