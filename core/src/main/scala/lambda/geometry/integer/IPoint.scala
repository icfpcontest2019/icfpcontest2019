package lambda.geometry.integer

import lambda.geometry.floating.FPoint

/**
  * A point with integer coordinates
  */
case class IPoint(x: Int, y: Int) {

  def +(p: IPoint) = IPoint(x + p.x, y + p.y)

  def -(p: IPoint) = IPoint(x - p.x, y - p.y)

  def *(d: Int) = IPoint(x * d, y * d)

  def /(d: Int) = IPoint(x / d, y / d)

  def length: Double = math.sqrt(x * x + y * y)

  def toPair: (Int, Int) = (x, y)

  def toFPoint: FPoint = FPoint(x, y)

  // Rotate CCW by pi/2
  def rotateLeft: IPoint = IPoint(-y, x)

  // Rotate CW by pi/2
  def rotateRight: IPoint = IPoint(y, -x)

  override def toString = s"($x,$y)"

  def toSquare: IPolygon = {
    val p1 = IPoint(x, y)
    val p2 = IPoint(x + 1, y)
    val p3 = IPoint(x + 1, y + 1)
    val p4 = IPoint(x, y + 1)
    IPolygon(List(p1, p2, p3, p4))
  }


}

