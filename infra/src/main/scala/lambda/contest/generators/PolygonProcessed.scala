package lambda.contest.generators

import java.awt.{Color, Graphics}

import lambda.geometry.floating.{FPoint, FPolygon}

/**
  * @author Ilya Sergey
  */
case class PolygonProcessed(polygon: FPolygon) {


  val MAX_FRAME_SIZE: Int = 600
  val MARGIN_SIZE: Int = 110
  val DOT_RADIUS: Int = 7

  val dimensions = getDimensions
  val bottomLeft = dimensions._1

  val topRight = dimensions._2

  val xSize = {
    val res = topRight._1 - bottomLeft._1
    assert(res > 0)
    res
  }

  val ySize = {
    val res = topRight._2 - bottomLeft._2
    assert(res > 0)
    res
  }

  // Compression Quotient
  val k = MAX_FRAME_SIZE / math.max(xSize, ySize)

  // Processed coordinates
  val processedVertices: Seq[(Int, Int)] = for (v <- polygon.vertices) yield processPoint(v)

  val frameSize: (Int, Int) = {
    val zSize = math.max(xSize, ySize)
    ((2 * MARGIN_SIZE + k * zSize).toInt,
      (2 * MARGIN_SIZE + k * zSize).toInt)
  }

  def processPoint(v: FPoint): (Int, Int) = {
    val (x1, y1) = (v.x - bottomLeft._1, ySize + bottomLeft._2 - v.y)
    val (x2, y2) = (x1 * k, y1 * k)
    val x3 = x2.toInt + MARGIN_SIZE
    val y3 = y2.toInt + MARGIN_SIZE / 2
    assert(x3 > 0 && y3 > 0)
    (x3, y3)
  }

  def processPoly(p: FPolygon): (Seq[Int], Seq[Int], Int) = {
    val ps = for (v <- p.vertices) yield processPoint(v)
    val unz = ps.unzip
    (unz._1, unz._2, ps.size)
  }

  def drawPoly(g: Graphics, p: FPolygon, c: Color): Unit = {
    g.setColor(c)
    val vs = processPoly(p)
    g.drawPolygon(vs._1.toArray, vs._2.toArray, vs._3)
  }

  def fillPoly(g: Graphics, p: FPolygon, c: Color): Unit = {
    g.setColor(c)
    val vs = processPoly(p)
    g.fillPolygon(vs._1.toArray, vs._2.toArray, vs._3)
  }

  def drawPoint(g: Graphics, pp: PolygonProcessed, p: FPoint, c: Color): Unit = {
    g.setColor(c)
    val (x, y) = pp.processPoint(p)
    g.fillOval(x - DOT_RADIUS / 2, y - DOT_RADIUS / 2, DOT_RADIUS, DOT_RADIUS)
  }

  private def getDimensions: ((Double, Double), (Double, Double)) = {
    val vs = polygon.vertices
    if (vs.isEmpty) return ((0, 0), (0, 0))
    val hd = vs.head
    val tl = vs.tail
    // Find bottom-left and top-right
    val res = tl.foldLeft((hd.x, hd.y), (hd.x, hd.y))((z, a) =>
      ((math.min(z._1._1, a.x), math.min(z._1._2, a.y)), (math.max(z._2._1, a.x), math.max(z._2._2, a.y)))
    )
    res
  }

  def fillWhiteBackground(g: Graphics): Unit = {
    g.setColor(Color.WHITE)
    val (x, y) = frameSize
    val z = math.max(x, y)
    g.fillPolygon(Array(0, 0, z, z), Array(0, z, z, 0), 4)
  }

}
