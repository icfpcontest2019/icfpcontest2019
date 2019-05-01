package lambda.geometry.integer

import lambda.geometry.floating.{Direction, FPoint, FSegment}

/**
  * @author Ilya Sergey
  */
object IntersectionUtils {

  /**
    * @param a left-bottom of the starting cell of the segment
    * @param b left-bottom of the ending cell of the segment
    * @return a list of square cells
    */
  def cellsIntersectedBySegment(a: IPoint, b: IPoint): List[IPoint] = {

    // Take the centers points and connect by a segment
    val shift = Direction(0.5, 0.5)
    val seg = FSegment(a.toFPoint + shift, b.toFPoint + shift)


    /* So, let us solve this in the following steps:

        1. Identify all cells "adjacent" to the segment
        2. Check which of them are intersected properly
        3. Check which of them are solid (i.e, parts of the walls)

 */

    val adjacent = adjacentCells(seg)
    val affected = adjacent.filter(segmentIntersectsCell(seg, _))
    affected
  }

  def adjacentCells(s: FSegment): List[IPoint] = {
    Nil
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
    intersections.size % 2 == 0
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
