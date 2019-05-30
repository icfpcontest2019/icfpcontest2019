package lambda.contest.blockchain

import lambda.geometry.integer.IPolygon

/**
  * @author Ilya Sergey
  */
object PuzzleCheckingUtils {

  val ratioDiscrepancy = 0.20

  def checkPolyForPuzzle(poly: IPolygon, boxSize: Int, minVertices: Int, maxVertices: Int): Boolean = {
    val (_, (dx, dy)) = poly.boundingBox
    val pixelDiscrepancy = math.floor(boxSize / 10)

    if (dx > boxSize || dy > boxSize) return false
    if (boxSize - dx > pixelDiscrepancy && boxSize - dy > pixelDiscrepancy) return false
    val vs = poly.vertices
    if (vs.size < minVertices || vs.size > maxVertices) return false
    val area = poly.toFPolygon.area
    val boxArea = boxSize * boxSize.toDouble
    val ratio = area / boxArea
    if (ratio < ratioDiscrepancy) return false
    true
  }



}
