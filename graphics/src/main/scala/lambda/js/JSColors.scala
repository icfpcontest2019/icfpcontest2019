package lambda.js

import lambda.contest.Booster
import org.scalajs.dom.ext.Color

/**
  * @author Ilya Sergey
  */
object JSColors {

  // Colors 
  val LIGHT_GRAY = Color(240, 240, 240)
  val DARK_GRAY = Color(64, 64, 64)
  val BLACK = Color(0, 0, 0)
  val RED = Color(255, 0, 0)
  val LIGHT_YELLOW = Color(255, 255, 204)
  // Gold
  val BATTERIES_COLOR = new Color(255, 204, 51)
  // Brown
  val COFFEE_COLOR = new Color(153, 102, 0)
  // Light green
  val DRILL_COLOR = new Color(0, 204, 0)
  // Purple
  val TELE_COLOR = new Color(102, 0, 153)
  // Light blue
  val CALL_WATCHMAN_COLOR = new Color(51, 153, 255)
  // Blue
  val CALL_POINT_COLOR = new Color(0, 0, 255)

  def boosterToColor(b: Booster.Value): Color = b match {
    case Booster.BatteriesBooster => BATTERIES_COLOR
    case Booster.CoffeeBooster => COFFEE_COLOR
    case Booster.DrillBooster => DRILL_COLOR
    case Booster.TeleBooster => TELE_COLOR
    case Booster.CallBooster => CALL_WATCHMAN_COLOR
    case Booster.CallPoint => CALL_POINT_COLOR
  }

}
