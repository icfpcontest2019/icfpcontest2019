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

  def toFPoint : FPoint = FPoint(x, y)
}
