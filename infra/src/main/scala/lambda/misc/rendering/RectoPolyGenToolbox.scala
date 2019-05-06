package lambda.misc.rendering

import java.awt.event.{ActionEvent, ActionListener}
import java.awt.{Button, Color, FlowLayout}

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

  def main(args: Array[String]) {

    renderPolygon

  }

  private def renderPolygon = {
    val pc = ContestRectilinearPolygonGenerator.generate().sample.get
    println(pc.pol.vertices.size)
    val ipol = IPolygon(pc.pol.vertices.map { case FPoint(x, y) => IPoint(x.toInt, y.toInt) })
    assert(ipol.isWellFormed && ipol.isRectilinear)
    drawShape(pc.pol)
    println(pc)
  }
  
  /**
    * Draw main shape 
    */
  def drawShape(p: FPolygon): Unit = {
    val frame = new JFrame()
    val pp = PolygonProcessed(p)

    val acceptButton = new JButton("Accept")
    acceptButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit ={
        println("Choo!")
      }
    })

    val rejectButton = new JButton("Reject")
    rejectButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit ={
        println("Broo!")
      }
    })

    frame.setLayout(new BoxLayout(frame.getContentPane, BoxLayout.Y_AXIS))
    frame.add(pp)
    frame.add(acceptButton)
    frame.add(rejectButton)
    pp.setVisible(true)

    // pane.setBackground(new Color(0, 0, 0))
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    val size = pp.frameSize
    frame.setSize(size._1, size._2)
    frame.setVisible(true)
  }


}




