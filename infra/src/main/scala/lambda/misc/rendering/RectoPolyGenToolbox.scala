package lambda.misc.rendering

import java.awt.event.{ActionEvent, ActionListener}
import java.awt.{Button, Color, FlowLayout, Graphics}

import javax.swing.{BoxLayout, JButton, JFrame, JPanel}
import lambda.geometry.floating.{FPoint, FPolygon}
import lambda.geometry.floating.generators.rectilinear.ContestRectilinearPolygonGenerator
import lambda.geometry.integer.{IPoint, IPolygon}

/**
  * Generating rectilinear polygons
  *
  * @author Ilya Sergey
  */

object RectoPolyGenToolbox {
  
  val cross2 = List((0, 0), (1, 0), (1, -2), (3, -2), (3, 0), (4, 0), (4, 1), (6, 1), (6, 3), (4, 3), (4, 4), (3, 4), (3, 6), (1, 6), (1, 4), (0, 4), (0, 3), (-2, 3), (-2, 1), (0, 1))

  def main(args: Array[String]) {
    draw()
  }

  private def generateNewPolygon = {
    val pc = ContestRectilinearPolygonGenerator.generate().sample.get
    println(pc.pol.vertices.size)
    val ipol = IPolygon(pc.pol.vertices.map { case FPoint(x, y) => IPoint(x.toInt, y.toInt) })
    assert(ipol.isWellFormed && ipol.isRectilinear)
    println(pc)
    pc
  }


  /**
    * Draw main shape 
    */
  def draw(): Unit = {

    val pc = generateNewPolygon
    var pp = PolygonProcessed(pc.pol)


    val frame = new JFrame()

    val polygonPanel = new JPanel() {
      override def paint(g: Graphics) = {
        pp.fillWhiteBackground(g)
        pp.fillPoly(g, pp.polygon, Color.LIGHT_GRAY)
      }
    }


    val acceptButton = new JButton("Accept")
    acceptButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        // println("Choo!")
        val newPoly = generateNewPolygon
        pp = PolygonProcessed(newPoly.pol)
        polygonPanel.paint(polygonPanel.getGraphics)
      }
    })

    val rejectButton = new JButton("Next")
    rejectButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
//        println("Broo!")
      }
    })

    frame.setLayout(new BoxLayout(frame.getContentPane, BoxLayout.Y_AXIS))
    frame.add(polygonPanel)
    frame.add(acceptButton)
    frame.add(rejectButton)
    
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    val size = pp.frameSize
    frame.setSize(size._1, size._2)
    frame.setVisible(true)
  }
}




