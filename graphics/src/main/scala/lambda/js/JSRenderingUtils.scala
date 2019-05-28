package lambda.js

import lambda.contest.{Booster, Cell, Watchman}
import lambda.contest.checkers.TaskMatrix
import lambda.geometry.integer.IPoint
import org.scalajs.dom.ext.Color

/**
  * @author Ilya Sergey
  */
object JSRenderingUtils {

  // Colors 
  val LIGHT_GRAY = Color(234, 234, 234)
  val DARK_GRAY = Color(64, 64, 64)
  val BLACK = Color(0, 0, 0)
  val RED = Color(255, 0, 0)
  val LIGHT_YELLOW = Color(255, 238, 115)
  val DARK_YELLOW = Color(237, 179, 0)
  // Gold
  val BATTERIES_COLOR = new Color(255, 204, 51)
  // Brown
  val COFFEE_COLOR = new Color(153, 102, 0)
  // Light green
  val DRILL_COLOR = new Color(0, 204, 0)
  // Purple
  val TELE_COLOR = new Color(147, 112, 219)
  // Installed Teleport (white)
  val INSTALLED_TELE_COLOR = Color(255, 255, 255)  // new Color(255, 20, 147)
  // Light blue
  val CALL_WATCHMAN_COLOR = new Color(51, 153, 255)
  // Blue
  val CALL_POINT_COLOR = new Color(0, 0, 255)
  
  // text colors
  val TEXT_GREEN = Color(125, 189, 125)
  val TEXT_YELLOW = Color(255, 255, 138)
  val TEXT_RED = Color(255, 148, 148)
  val TEXT_WHITE = Color(255, 255, 255)

  def boosterToColor(b: Booster.Value): Color = b match {
    case Booster.BatteriesBooster => BATTERIES_COLOR
    case Booster.CoffeeBooster => COFFEE_COLOR
    case Booster.DrillBooster => DRILL_COLOR
    case Booster.TeleBooster => TELE_COLOR
    case Booster.CallBooster => CALL_WATCHMAN_COLOR
    case Booster.CallPoint => CALL_POINT_COLOR
  }

  def getAffectedCells(matrix: TaskMatrix, dx: Int, dy: Int,
                       affectedRadius: Int,
                       wPos: IPoint, w: Watchman): Set[(IPoint, Cell)] = {
    val (x0, y0) = wPos.toPair

    // Getting the torch spread
    val torchX = w.getTorchRange(IPoint(0, 0)).maxBy { p => math.abs(p.x) }.x
    val torchY = w.getTorchRange(IPoint(0, 0)).maxBy { p => math.abs(p.y) }.y
    // Adding because of coffee booster 
    val torchRadius = math.max(math.abs(torchX), math.abs(torchY)) * 2 + 3
    val maxRadius = math.max(torchRadius, affectedRadius)

    // Getting the square to re-render
    val xmin = x0 - maxRadius
    val max = x0 + maxRadius
    val ymin = y0 - maxRadius
    val ymax = y0 + maxRadius

    (for {x <- xmin to max
          y <- ymin to ymax
          p = IPoint(x, y)} yield {
      if (x < 0 || x >= dx || y < 0 || y >= dy) (p, Cell())
      else (p, matrix(x)(y))
    }).toSet
  }

  def getAffectedNeighbours(p: IPoint, affectedRadius: Int,
                            matrix: TaskMatrix, dx: Int, dy: Int): Set[(IPoint, Cell)] = {
    val (x0, y0) = p.toPair
    val xmin = x0 - affectedRadius
    val max = x0 + affectedRadius
    val ymin = y0 - affectedRadius
    val ymax = y0 + affectedRadius

    (for {x <- xmin to max
          y <- ymin to ymax
          p = IPoint(x, y)} yield {
      if (x < 0 || x >= dx || y < 0 || y >= dy) (p, Cell())
      else (p, matrix(x)(y))
    }).toSet
  }

}
