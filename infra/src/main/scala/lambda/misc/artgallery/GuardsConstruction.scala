package lambda.misc.artgallery

import lambda.geometry.floating.{FPoint, FPolygon}

/**
  * Computing the set of guards
  *
  * @author Ilya Sergey
  */

/**
  * Just take every second vertex of the polygon
  */
object DumbGuards {

  def dumbVisibility(p: FPolygon): Option[Seq[FPoint]] = {
    val vs = p.vertices
    // Degenerate case
    val n = vs.size
    if (n <= 2) return None

    Some(for (i <- 0 until (vs.size - 2)) yield vs(i))
  }

}
