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

  def toPair: (Int, Int) = (x, y)

  def toFPoint : FPoint = FPoint(x, y)

  // Rotate CCW by pi/2
  def rotateLeft: IPoint = IPoint(-y, x)

  // Rotate CW by pi/2
  def rotateRight: IPoint = IPoint(y, -x)

  override def toString = s"($x,$y)"
}
