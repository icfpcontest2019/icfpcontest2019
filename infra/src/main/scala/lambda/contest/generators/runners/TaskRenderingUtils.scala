package lambda.contest.generators.runners

import java.awt.{Color, Graphics}
import java.io.File

import lambda.contest.generators.PolygonToRender
import lambda.contest.{Booster, ContestTask}
import lambda.geometry.floating.Direction

/**
  * @author Ilya Sergey
  */
object TaskRenderingUtils {

  def renderTask(g: Graphics, t: ContestTask, f: File, firstObst: Boolean = false): Unit = {
    val ContestTask(room, init, obs, boosters) = t

    // Render main polygon
    val pp = PolygonToRender(room.toFPolygon)
    pp.fillWhiteBackground(g)
    pp.fillPoly(g, room.toFPolygon, Color.LIGHT_GRAY)
    pp.drawPoly(g, room.toFPolygon, Color.BLACK)

    // Render obstacles
    obs match {
      case Nil =>
      case head :: tl =>
        // Dark yellow
        val color = if (firstObst) new Color(255, 204, 0) else new Color(255, 255, 204) 
        pp.fillPoly(g, head.toFPolygon, color)
        pp.drawPoly(g, head.toFPolygon, Color.BLACK)
        tl.foreach { o =>
          // Very light yellow
          pp.fillPoly(g, o.toFPolygon, new Color(255, 255, 204))
          pp.drawPoly(g, o.toFPolygon, Color.BLACK)
        }
    }

    // Render initial position
    pp.drawPoint(g, init.toFPoint + Direction(0.5, 0.5), Color.RED)

    // Draw boosters
    for ((b, bp) <- boosters) {
      pp.drawPoint(g, bp.toFPoint + Direction(0.5, 0.5), boosterToColor(b))
    }

    // Write file name
    val text = f.getName
    g.setColor(Color.BLACK)
    g.drawChars(text.toCharArray, 0, text.length, 10, 10)
    val (x, y) = room.dimensions
    val dims = room.dimensions.toString
    val prod = (x * y).toString
    g.drawChars(dims.toCharArray, 0, dims.length, 10, 30)
    g.drawChars(prod.toCharArray, 0, prod.length, 10, 50)

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
