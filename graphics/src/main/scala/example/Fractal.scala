package example

import lambda.geometry.integer.IPoint
import org.scalajs.dom
import org.scalajs.dom.html

import scala.scalajs.js.annotation.JSExportTopLevel
import scala.util.Random

object Fractal {
  @JSExportTopLevel("drawFractal")
  def main(canvas: html.Canvas): Unit = {
    val ctx = canvas.getContext("2d")
      .asInstanceOf[dom.CanvasRenderingContext2D]

    var count = 0
    var p = IPoint(0, 0)
    val corners = Seq(IPoint(255, 255), IPoint(0, 255), IPoint(128, 0))

    def clear() = {
      ctx.fillStyle = "black"
      ctx.fillRect(0, 0, 255, 255)
    }

    def run = for (i <- 0 until 10) {
      if (count % 3000 == 0) clear()
      count += 1
      p = (p + corners(Random.nextInt(3))) / 2

      val height = 512.0 / (255 + p.y)
      val r = (p.x * height).toInt
      val g = ((255 - p.x) * height).toInt
      val b = p.y
      ctx.fillStyle = s"rgb($g, $r, $b)"

      ctx.fillRect(p.x, p.y, 1, 1)
    }

    dom.window.setInterval(() => run, 30)
  }

}