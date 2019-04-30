package lambda.geometry

package object integer {

  def mkIPoly(points: (Int, Int)*) =
    IPolygon(points.map {case (x, y) => IPoint(x, y)})

}
