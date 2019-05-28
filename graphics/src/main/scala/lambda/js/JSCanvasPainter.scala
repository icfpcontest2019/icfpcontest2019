package lambda.js

import lambda.contest.Cell
import lambda.geometry.floating.{FPoint, FPolygon, RenderUtils}
import lambda.geometry.integer.{IPoint, IPolygon}
import lambda.js.JSRenderingUtils._
import org.scalajs.dom
import org.scalajs.dom.ext.Color

/**
  * @author Ilya Sergey
  */
class JSCanvasPainter(ctx: dom.CanvasRenderingContext2D,
                      room: IPolygon,
                      xMax: Int, yMax: Int, upperBoundary: Int = 0) {


  val MARGIN_SIZE: Int = 10

  val xSize = xMax - 2 * MARGIN_SIZE
  val ySize = yMax - 2 * MARGIN_SIZE - upperBoundary

  val defaultRadius = 4

  lazy val scalingCoefficient: Double = {
    val (_, (px, py)) = room.shiftToOrigin.boundingBox
    val kx = xSize.toDouble / px
    val ky = ySize.toDouble / py
    math.min(kx, ky)
  }

  lazy val affectedRadius: Int = (defaultRadius.toDouble / scalingCoefficient + 1).toInt

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

  def squareToRect(sq: IPoint) = {
    val (x, y) = movePoint(sq.toFPoint).toPair
    (x, y, scalingCoefficient, scalingCoefficient)
  }

  def renderCell(p: IPoint, c: Cell): Unit = {
    val (x, y) = movePoint(p.toFPoint).toPair
    // Do not render outside of canvas boundaries
    if (x < 0 || x > xMax || y < 0 || y > yMax) return 
    
    val color = if (!c.canStep) DARK_GRAY
    else if (c.isIlluminated) LIGHT_YELLOW
    else LIGHT_GRAY
    ctx.fillStyle = color.toHex
    drawPoly(p.toSquare, color)
    ctx.fillStyle = BLACK.toHex
  }

  def renderCellWithColor(p: IPoint, color: Color): Unit = {
    val (x, y) = movePoint(p.toFPoint).toPair
    // Do not render outside of canvas boundaries
    if (x < 0 || x > xMax || y < 0 || y > yMax) return 
    ctx.fillStyle = color.toHex
    drawPoly(p.toSquare, color)
    ctx.fillStyle = BLACK.toHex
  }

}
