package lambda.contest.generators

import java.awt.event.{ActionEvent, ActionListener}
import java.awt.{Color, Graphics}

import javax.swing.{BoxLayout, JButton, JFrame, JPanel}
import lambda.geometry.floating.generators.CompositePolygon

/**
  * @author Ilya Sergey
  */
trait GeneratorRendering {

  protected def generateNewPolygon(boxSize: Int = 100): CompositePolygon

  /**
    * Draw the generator 
    */
  def draw(boxSize: Int): Unit = {

    val pc = generateNewPolygon(boxSize)
    var pp = PolygonProcessed(pc.pol)


    val frame = new JFrame()
    val polygonPanel = new JPanel() {
      override def paint(g: Graphics) = {
        pp.fillWhiteBackground(g)
        pp.fillPoly(g, pp.polygon, Color.LIGHT_GRAY)
      }
    }

    val acceptButton = new JButton("Accept")
    acceptButton.addActionListener((e: ActionEvent) => {
      pp = PolygonProcessed(generateNewPolygon(boxSize).pol)
      polygonPanel.paint(polygonPanel.getGraphics)
    })

    val rejectButton = new JButton("Next")
    rejectButton.addActionListener((e: ActionEvent) => {
      // TODO: Implement me!!
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
