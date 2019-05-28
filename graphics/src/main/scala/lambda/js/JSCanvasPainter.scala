package lambda.js

import lambda.geometry.floating.{FPoint, FPolygon, RenderUtils}
import lambda.geometry.integer.{IPoint, IPolygon}
import lambda.js.JSRenderingUtils.{BLACK, DARK_GRAY}
import org.scalajs.dom
import org.scalajs.dom.ext.Color

/**
  * @author Ilya Sergey
  */
class JSCanvasPainter(ctx: dom.CanvasRenderingContext2D,
                      room: IPolygon,
                      dx: Int, dy: Int, upperBoundary: Int = 0) {


  val MARGIN_SIZE: Int = 10

  val xSize = dx - 2 * MARGIN_SIZE
  val ySize = dy - 2 * MARGIN_SIZE - upperBoundary

  lazy val scalingCoefficient: Double = {
    val (_, (px, py)) = room.shiftToOrigin.boundingBox
    val kx = xSize.toDouble / px
    val ky = ySize.toDouble / py
    math.min(kx, ky)
  }

  lazy val shift: FPoint = {
    val scaledPoly = FPolygon(room.vertices.map(v => v.toFPoint * scalingCoefficient))
    val (_, FPoint(bx, by)) = RenderUtils.getPolygonBoundingBox(scaledPoly)
    val myMiddle = FPoint(xSize / 2 + MARGIN_SIZE, (ySize - upperBoundary) / 2 + MARGIN_SIZE)
    val shift = myMiddle - FPoint(bx / 2, by / 2)
    shift
  }

  def movePoint(p: FPoint) = p match {
    case FPoint(x, y) =>
      val k = scalingCoefficient
      val x1 = x * k + shift.x
      val y1 = ySize - (y * k + shift.y) + upperBoundary
      FPoint(x1, y1)
  }

  val defaultRadius = 4

  def drawCirclePoint(p: IPoint, c: Color): Unit = {
    ctx.fillStyle = c.toHex
    val pos = movePoint(p.toFPoint + FPoint(0.5, 0.5)) 
    ctx.beginPath()
    ctx.arc(pos.x, pos.y, defaultRadius, 0, 2 * scala.math.Pi)
    ctx.fill()
    ctx.fillStyle = BLACK.toHex
    ctx.stroke()
    ctx.fillStyle = DARK_GRAY.toHex
  }

  def drawPoly(poly: IPolygon, c: Color, stroke: Boolean = false): Unit = {
    val k = scalingCoefficient
    val scaledVerties = poly.vertices.map(p => movePoint(p.toFPoint))
    if (scaledVerties.size < 3) return
    val h :: t = scaledVerties
    ctx.fillStyle = c.toHex
    ctx.beginPath
    ctx.moveTo(h.x, h.y)
    t.foreach { case FPoint(x, y) => ctx.lineTo(x, y) }
    ctx.closePath()
    ctx.fill()
    if (stroke) {
      ctx.strokeStyle = "#000000"
      ctx.lineWidth = 1
      ctx.stroke()
    }

    ctx.fillStyle = DARK_GRAY.toHex
  }

}
