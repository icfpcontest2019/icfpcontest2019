package lambda.contest.generators.runners

import java.awt.Color.BLACK
import java.awt.{Color, Graphics}
import java.io.File

import lambda.contest.generators.PolygonToRender
import lambda.contest.{Booster, ContestTask}
import lambda.geometry.floating.Direction
import lambda.geometry.integer.IPoint

/**
  * @author Ilya Sergey
  */
object TaskRenderingUtils {

  def renderTask(g: Graphics, t: ContestTask, f: File,
                 firstObst: Boolean = false,
                 inOutPoints: (List[IPoint], List[IPoint]) = (Nil, Nil)): Unit = {
    val ContestTask(room, init, obs, boosters) = t

    // Render main polygon
    val pp = PolygonToRender(room.toFPolygon)
    pp.fillWhiteBackground(g)
    pp.fillPoly(g, room.toFPolygon, Color.LIGHT_GRAY)
    pp.drawPoly(g, room.toFPolygon, BLACK)

    // Render obstacles
    obs match {
      case Nil =>
      case head :: tl =>
        // Dark yellow
        val color = if (firstObst) new Color(255, 204, 0) else new Color(255, 255, 204)
        pp.fillPoly(g, head.toFPolygon, color)
        pp.drawPoly(g, head.toFPolygon, BLACK)
        tl.foreach { o =>
          // Very light yellow
          pp.fillPoly(g, o.toFPolygon, new Color(255, 255, 204))
          pp.drawPoly(g, o.toFPolygon, BLACK)
        }
    }

    // Render initial position
    pp.drawPoint(g, init.toFPoint + Direction(0.5, 0.5), Color.RED)

    // Draw boosters
    for ((b, bp) <- boosters) {
      pp.drawPoint(g, bp.toFPoint + Direction(0.5, 0.5), boosterToColor(b))
    }

    // Write file name


    val text = (f.getName, BLACK)
    val (x, y) = room.dimensions
    val dims = (s"Dimensions: ${x}x${y}", BLACK)

    // val prod = (x * y).toString
    val bs = boosters.groupBy { case (b, n) => b }
      .toList
      .map { case (b, l) => (b, l.length) }
      .sortBy(_._1.toString)
      .map { case (b, n) => (s"${Booster.pp(b)}: $n", boosterToColor(b)) }

    val lines = List(text, dims) ++ bs

    for (i <- lines.indices) {
      val (line, color) = lines(i)
      g.setColor(color)
      g.drawChars(line.toCharArray, 0, line.length, 10, 5 + (2 * i + 1) * 10)
    }

    val (inPoints, outPoints) = inOutPoints
    for (p <- inPoints) {
      val sq = p.toSquare.toFPolygon
      pp.fillPoly(g, sq, Color.GREEN)
      pp.drawPoly(g, sq, Color.BLACK)
    }

    for (p <- outPoints) {
      val sq = p.toSquare.toFPolygon
      pp.fillPoly(g, sq, Color.RED)
      pp.drawPoly(g, sq, Color.BLACK)
    }

  }

  // Gold
  val BATTERIES_COLOR = new Color(255, 204, 51)
  // Brown
  val COFFEE_COLOR = new Color(153, 102, 0)
  // Light green
  val DRILL_COLOR = new Color(0, 204, 0)
  // Purple
  val TELEPORT_COLOR = new Color(102, 0, 153)
  // Light blue
  val CALL_WATCHMAN_COLOR = new Color(51, 153, 255)
  // Blue
  val CALL_POINT_COLOR = new Color(0, 0, 255)

  def boosterToColor(b: Booster.Value): Color = b match {
    case Booster.BatteriesBooster => BATTERIES_COLOR
    case Booster.CoffeeBooster => COFFEE_COLOR
    case Booster.DrillBooster => DRILL_COLOR
    case Booster.TeleBooster => TELEPORT_COLOR
    case Booster.CallBooster => CALL_WATCHMAN_COLOR
    case Booster.CallPoint => CALL_POINT_COLOR
  }


}
