package lambda.geometry

package object integer {

  def mkIPoly(points: (Int, Int)*) =
    IPolygon(points.map {case (x, y) => IPoint(x, y)})

  implicit val _iPointOrdering: Ordering[IPoint] = new Ordering[IPoint] {
    override def compare(a: IPoint, b: IPoint) = {
      if (a.x < b.x) -1
      else if (a.x > b.x) 1
      else a.y.compare(b.y)
    }
  }
}
