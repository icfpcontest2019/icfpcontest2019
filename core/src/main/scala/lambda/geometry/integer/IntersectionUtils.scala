package lambda.geometry.integer

import lambda.geometry.floating.{Direction, FPoint, FSegment}

import scala.math.{ceil, floor, max, min}

/**
  * @author Ilya Sergey
  */
object IntersectionUtils {

  /**
    * @param a left-bottom of the starting cell of the segment
    * @param b left-bottom of the ending cell of the segment
    * @return a list of square cells
    *
    *
    * Notice that this function allows for a "diagonal penetration":
    *
    * X | b
    * ------
    * a | Y
    *
    * If X and Y are obstacles, a will still "see" b, since neither X
    * not Y are considered to be intersected.
    *
    */
  def cellsIntersectedByViewSegment(a: IPoint, b: IPoint): List[IPoint] = {
    // Take the centers points and connect by a segment
    val shift = Direction(0.5, 0.5)
    val seg = FSegment(a.toFPoint + shift, b.toFPoint + shift)
    cellsCrossedBySegment(seg)
  }


  /**
    * An auxiliary function that works with general segments
    */
  def cellsCrossedBySegment(s: FSegment): List[IPoint] = {
    val (a, b) = s.toPair
    val res =
      if (a.x == b.x) {
        // Vertical. vector
        val x = floor(a.x).toInt // Same as b.x
        for (y <- ceil(min(a.y, b.y)).toInt to floor(max(a.y, b.y)).toInt)
          yield IPoint(x, y)
      } else if (a.y == b.y) {
        // Horizontal vector
        val y = floor(a.y).toInt // Same as b.y
        for (x <- ceil(min(a.x, b.x)).toInt to floor(max(a.x, b.x)).toInt)
          yield IPoint(x, y)
      } else {
        // Segment directional vector
        val (p0, p1) = if (a.x < b.x) (a, b) else (b, a)
        val FPoint(rx, ry) = p1 - p0
        // A slope
        for {
          x <- floor(p0.x).toInt to ceil(p1.x).toInt
          t1 = (x - p0.x) / rx
          t2 = (x + 1 - p0.x) / rx
          yy1 = p0.y + ry * t1
          yy2 = p0.y + ry * t2
          y1 = floor(min(yy1, yy2)).toInt
          y2 = ceil(max(yy1, yy2)).toInt
          y <- y1 to y2
          ty = (y - p0.y) / ry
        } yield IPoint(x, y)
      }
    val ia = IPoint(floor(a.x).toInt, floor(a.y).toInt)
    val ib = IPoint(floor(b.x).toInt, floor(b.y).toInt)
    val overApprox = (ia :: ib :: res.toList).distinct
    val almost = overApprox.filter(segmentIntersectsCell(s, _))
    almost.sorted
  }

  // Assuming that none of the ends of the segment is within the cell
  def segmentIntersectsCell(s: FSegment, cell: IPoint): Boolean = {
    val FSegment(sa, sb) = s

    if (pointWithinCell(sa, cell) || pointWithinCell(sb, cell)) return true

    val es = getCellEdges(cell)
    val intersections =
      (for {e <- es
            i = e.intersection(s)
            if i.isDefined
            Some(z) = i} yield z).distinct
    intersections.nonEmpty && intersections.size % 2 == 0
  }

  private def pointWithinCell(p: FPoint, cell: IPoint): Boolean = {
    val FPoint(px, py) = p
    val (x, y) = cell.toPair
    (x <= px && px <= x + 1) &&
      (y <= py && py <= y + 1)

  }

  private def getCellEdges(cell: IPoint): List[FSegment] = {
    val FPoint(x, y) = cell.toFPoint
    val e1 = FSegment(FPoint(x, y), FPoint(x + 1, y))
    val e2 = FSegment(FPoint(x + 1, y), FPoint(x + 1, y + 1))
    val e3 = FSegment(FPoint(x + 1, y + 1), FPoint(x, y + 1))
    val e4 = FSegment(FPoint(x, y + 1), FPoint(x, y))
    List(e1, e2, e3, e4)
  }


}
