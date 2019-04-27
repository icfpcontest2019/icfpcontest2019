package lambda.misc.rendering

import java.awt.{Color, Graphics}
import java.io.File

import lambda.geometry.floating.visibility.JoeSimpsonVisibility._
import lambda.geometry.floating.visibility.VisibilityChecker._
import lambda.geometry.floating.{FPoint, FPolygon}

import scala.io.Source

/**
  * @author Ilya Sergey
  */

object ArtGalleryPainter {

  val MAX_FRAME_SIZE = 650
  val MARGIN_SIZE = 100
  val DOT_RADIUS = 7


  def main(args: Array[String]): Unit = {
    if (args.isEmpty) {
      println("No file to process for Art Gallery Problem.")
      return
    }
    val filename = args.head
    val file = new File(filename)
    if (!file.exists()) {
      println("No file with the given path found.")
      return
    }

    val input = Source.fromFile(filename).mkString("")

    val res = PolygonParser(input)

    if (!res.successful) {
      println("Parsing the polygon failed. Check examples in ./src/test/resources/.")
      return
    }

    val (vs, guards, b) = res.get
    val pol = FPolygon(vs)

    drawShape(pol, guards.toSeq, b, guards.nonEmpty)

    println(s"Polygon size: ${pol.vertices.size}")

    val shortestEdge = pol.edges.minBy(s => s.length)

    assert(pol.containsPoint(FPoint(0, 0)))
    assert(pol.containsPoint(FPoint(1, 0)))
    assert(pol.containsPoint(FPoint(0, 1)))
    assert(pol.containsPoint(FPoint(1, 1)))


    println(s"Polygon size: ${pol.vertices.size}")
    println(s"Shortest edge: ${shortestEdge.length}")

  }

  // Drawing stuff
  import javax.swing.{JFrame, JPanel}

  case class PolygonProcessed(polygon: FPolygon) {

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

    val frameSize: (Int, Int) = ((2 * MARGIN_SIZE + k * xSize).toInt,
        (2 * MARGIN_SIZE + k * ySize).toInt)

    def processPoint(v: FPoint): (Int, Int) = {
      val (x1, y1) = (v.x - bottomLeft._1, ySize + bottomLeft._2 - v.y)
      val (x2, y2) = (x1 * k, y1 * k)
      val x3 = x2.toInt + MARGIN_SIZE
      val y3 = y2.toInt + MARGIN_SIZE
      assert(x3 > 0 && y3 > 0)
      (x3, y3)
    }

    def processPoly(p: FPolygon): (Seq[Int], Seq[Int], Int) = {
      val ps = for (v <- p.vertices) yield processPoint(v)
      val unz = ps.unzip
      (unz._1, unz._2, ps.size)
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

  }

  def drawShape(p: FPolygon, ps: Seq[FPoint], drawTriang: Boolean = false, drawCE: Boolean = true): Unit = {
    val frame = new JFrame()
    val pp = PolygonProcessed(p)
    val pane = frame.getContentPane
    pane.add(new PolyClass(pp, ps, drawTriang, drawCE))
    pane.setBackground(new Color(0, 0, 0))
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    val size = pp.frameSize
    frame.setSize(size._1, size._2)
    frame.setVisible(true)
  }

  class PolyClass(pp: PolygonProcessed, ps: Seq[FPoint], drawTriang: Boolean, drawCE: Boolean) extends JPanel {

    override def paint(g: Graphics): Unit = {

      fillWhiteBackground(pp, g)

      fillPoly(g, pp.polygon, pp, Color.LIGHT_GRAY)

      val vps = for (z <- ps; vpr = visibilityPolygon(pp.polygon, z); if vpr.isDefined; vp = vpr.get) yield vp
      // VP Fillings
      for (vp <- vps) {
        fillPoly(g, vp, pp, Color.PINK)
      }
      // VP boundaries
      //      for (vp <- vps) {
      //        drawPoly(g, vp, pp, Color.ORANGE)
      //      }

      // Polygon itself
      drawPoly(g, pp.polygon, pp, Color.BLACK)

      // Counterexample (if exists)
      val (_, res, ts) = checkVisibility(pp.polygon, ps)

      // draw triangluation
      if (drawTriang) {
        for (t <- ts) {
          drawPoly(g, t, pp, Color.GRAY)
        }
      }

      // guards
      for (z <- ps) {
        drawPoint(g, pp, z, Color.BLUE)
      }
      if (res.isDefined && res.get._2 == FailReason.NotVisible && drawCE) {
        val c = res.get._1
        drawPoint(g, pp, c, Color.RED)
      }

    }

    def drawPoly(g: Graphics, p: FPolygon, pp: PolygonProcessed, c: Color): Unit = {
      g.setColor(c)
      val vs = pp.processPoly(p)
      g.drawPolygon(vs._1.toArray, vs._2.toArray, vs._3)
    }

    def fillPoly(g: Graphics, p: FPolygon, pp: PolygonProcessed, c: Color): Unit = {
      g.setColor(c)
      val vs = pp.processPoly(p)
      g.fillPolygon(vs._1.toArray, vs._2.toArray, vs._3)
    }

    def drawPoint(g: Graphics, pp: PolygonProcessed, p: FPoint, c: Color): Unit = {
      g.setColor(c)
      val (x, y) = pp.processPoint(p)
      g.fillOval(x - DOT_RADIUS / 2, y - DOT_RADIUS / 2, DOT_RADIUS, DOT_RADIUS)
    }

  }

  def fillWhiteBackground(pp: PolygonProcessed, g: Graphics): Unit = {
    g.setColor(Color.WHITE)
    val (x, y) = pp.frameSize
    g.fillPolygon(Array(0, 0, x, x), Array(0, y, y, 0), 4)
  }

}
