package lambda.geometry

import java.awt.{BasicStroke, Color, Dimension, Graphics, Graphics2D}
import javax.swing.{JFrame, JPanel}

import SegmentUtils._

/**
  * @author Ilya Sergey
  */

object RenderUtils {
  // Gets the bounding box of a sequence of points.
  def getPointsBoundingBox(points: Seq[Point2D]): (Point2D, Point2D) = {
    points.foldLeft((Point2D.MaxValue, Point2D.MinValue))(
      (candidate, box) => (candidate._1 glb box, candidate._2 lub box)
    )
  }

  // Gets the bounding box of a polygon.
  def getPolygonBoundingBox(polygon: Polygon): (Point2D, Point2D) = {
    getPointsBoundingBox(polygon.vertices)
  }

  // Gets the bounding box of a sequence of polygons.
  def getPolygonBoundingBox(polygons: Seq[Polygon]): (Point2D, Point2D) =
    polygons.foldLeft(Point2D.MaxValue, Point2D.MinValue)(
      (bound, polygon) => {
        var polygonBox = getPolygonBoundingBox(polygon)
        (bound._1 glb polygonBox._1, bound._2 lub polygonBox._2)
      }
    )

  // Gets the bounding box of a segment.
  def getSegmentBoundingBox(segment: Segment): (Point2D, Point2D) =
    (segment._1 glb segment._2, segment._1 lub segment._2)

  // Gets the bounding box of a series of segments.
  def getSegmentBoundingBox(segments: Seq[Segment]): (Point2D, Point2D) = {
    segments.foldLeft(Point2D.MaxValue, Point2D.MinValue)(
      (bound, segment) => {
        var segmentBox = getSegmentBoundingBox(segment)
        (bound._1 glb segmentBox._1, bound._2 lub segmentBox._2)
      }
    )
  }

  def getGraphicParameters(p: Polygon) = {
    val (botl, topr) = getPolygonBoundingBox(p)
    val mid = (botl, topr).middle
    val width = topr.x - botl.x
    val height = topr.y - botl.y
    // Scale coefficient
    val k = 600 / math.max(width, height)

    // Overall graphic area is within 800 * 800 pixels
    val xShift = k * (mid.x - botl.x)
    val yShift = k * (mid.y - botl.y)
    val gWidth = k * width + 200
    val gHeigth = k * height + 200

    (gWidth.toInt, gHeigth.toInt, k.toInt, xShift.toInt, 200 + yShift.toInt)
  }

  trait DrawingPanel extends JPanel {
    val COLORS = List(
      Color.red,
      Color.green,
      Color.blue,
      Color.yellow,
      Color.orange,
      Color.pink,
      Color.magenta
    )

    // Point radius as a fraction of the dimension
    val POINT_RADIUS = 1.5 / 200

    def drawComponents(graphics: Graphics): Unit = {
      // Objects we are supposed to draw
      val polygonBorders = polygonBordersToDraw()
      var polygons = polygonsToDraw()
      var segments = segmentsToDraw()
      var points = pointsToDraw()
      val allPolygons = polygons ++ polygonBorders

      // Work our bounding box
      var (topLeftPoly, bottomRightPoly) = RenderUtils.getPolygonBoundingBox(allPolygons.map(_._1))
      var (topLeftPoint, bottomRightPoint) = RenderUtils.getPointsBoundingBox(points.map(_._1))
      var (topLeftSegment, bottomRightSegment) = RenderUtils.getSegmentBoundingBox(segments.map(_._1))

      var topLeft = (topLeftPoly glb topLeftPoint glb topLeftSegment).-(Point2D(1, 1))
      var bottomRight = (bottomRightPoly lub bottomRightPoint lub bottomRightSegment).-(Point2D(-1, -1))

      val xSpan = bottomRight.x - topLeft.x
      val ySpan = bottomRight.y - topLeft.y

      // Helpers that map a point to a coordinate of the canvas
      def mapX(point: Point2D) = ((point.x - topLeft.x) / xSpan * getWidth).toInt

      def mapY(point: Point2D) = ((point.y - topLeft.y) / ySpan * getHeight).toInt

      // Get an empty canvas first
      graphics.setColor(Color.white)
      graphics.fillPolygon(Array(0, 0, getWidth, getWidth), Array(0, getHeight, getHeight, 0), 4)

      //Draw only polygon borders
      for ((polygonBorder, lineColor) <- polygonBorders) {

        val xPoints = polygonBorder.vertices.map(mapX).toArray
        val yPoints = polygonBorder.vertices.map(mapY).toArray
        val nPoints = polygonBorder.vertices.length

        graphics.setColor(lineColor)
        graphics.drawPolygon(xPoints, yPoints, nPoints)
      }

      // Draw the polygons
      for ((polygon, color) <- polygons) {
        graphics.setColor(color)
        graphics.fillPolygon(
          polygon.vertices.map(mapX).toArray,
          polygon.vertices.map(mapY).toArray,
          polygon.vertices.length
        )
      }

      // Draw the segments
      for ((segment, color) <- segments) {
        graphics.setColor(color)
        graphics.asInstanceOf[Graphics2D].setStroke(new BasicStroke(2))
        graphics.drawLine(mapX(segment._1), mapY(segment._1), mapX(segment._2), mapY(segment._2))
      }

      // Draw the points
      for ((point, color) <- points) {
        graphics.setColor(color)
        graphics.fillOval(
          (mapX(point) - getWidth * POINT_RADIUS / 2).toInt,
          (mapY(point) - getHeight * POINT_RADIUS / 2).toInt,
          (getWidth * POINT_RADIUS).toInt,
          (getHeight * POINT_RADIUS).toInt
        )
      }
    }

    override def paint(graphics: Graphics): Unit = {
      super.paint(graphics)
      drawComponents(graphics)
    }

    def polygonsToDraw(): Seq[(Polygon, Color)]

    def polygonBordersToDraw(): Seq[(Polygon, Color)]

    def segmentsToDraw(): Seq[(Segment, Color)]

    def pointsToDraw(): Seq[(Point2D, Color)]
  }

  def display(panel: DrawingPanel, frameTitle: String = "") = {
    val frame = new JFrame(frameTitle);
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    frame.setMinimumSize(new Dimension(900, 900))
    panel.setSize(100, 100)
    frame.add(panel);
    frame.pack();
    frame.setVisible(true);
  }
}
