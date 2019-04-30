package lambda

import lambda.geometry.integer.IPoint

/**
  * @author Ilya Sergey
  */
object ContestTestUtils {

  def roomWithinBoundingBox(room: _root_.lambda.geometry.integer.IPolygon) = {
    val ((xl, yl), (xr, yr)) = room.boundingBox

    assert(xl >= 0 && yl >= 0 && xl < xr && yl < yr)

    assert(room.vertices.forall { case IPoint(x, y) =>
      xl <= x && x <= xr &&
        yl <= y && y <= yr
    })
  }


}
