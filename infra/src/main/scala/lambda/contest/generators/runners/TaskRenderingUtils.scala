package lambda.contest.generators.runners

import java.awt.{Color, Graphics}
import java.io.File

import javax.swing.JButton
import lambda.contest.{Booster, ContestTask}
import lambda.contest.generators.PolygonToRender

/**
  * @author Ilya Sergey
  */
object TaskRenderingUtils {

  def renderTask(g: Graphics, t: ContestTask, f: File): Unit = {
    val ContestTask(room, init, obs, boosters) = t

    // Render main polygon
    val pp = PolygonToRender(room.toFPolygon)
    pp.fillWhiteBackground(g)
    pp.fillPoly(g, room.toFPolygon, Color.LIGHT_GRAY)

    // Render obstacles
    obs match {
      case Nil =>
      case head :: tl =>
        // Dark yellow
        pp.fillPoly(g, head.toFPolygon, new Color(255, 204, 0))
        pp.drawPoly(g, head.toFPolygon, Color.BLACK)
        tl.foreach { o =>
          // Very light yellow
          pp.fillPoly(g, o.toFPolygon, new Color(255, 255, 204))
          pp.drawPoly(g, o.toFPolygon, Color.BLACK)
        }
    }

    // Render initial position
    pp.fillPoly(g, init.toSquare.toFPolygon, Color.RED)

    // Draw boosters
    for ((b, bp) <- boosters) {
      pp.fillPoly(g, bp.toSquare.toFPolygon, boosterToColor(b))
    }

    // Write file name
    val text = f.getName
    g.setColor(Color.BLACK)
    g.drawChars(text.toCharArray, 0, text.length, 10, 10)

    pp.drawPoly(g, room.toFPolygon, Color.BLACK)
  }

  def boosterToColor(b: Booster.Value): Color = b match {
    case Booster.BatteriesBooster =>
      // Gold
      new Color(255, 204, 51)
    case Booster.CoffeeBooster =>
      // Brown
      new Color(153, 102, 0)
    case Booster.DrillBooster =>
      // Light green
      new Color(0, 204, 0)
    case Booster.TeleportBooster =>
      // Purple
      new Color(102, 0, 153)
    case Booster.CallWatchmanBooster =>
      // Light blue
      new Color(51, 153, 255)
    case Booster.CallPoint =>
      // Blue
      new Color(0, 0, 255)
  }


}
