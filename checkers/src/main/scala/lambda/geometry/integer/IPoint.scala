package lambda.geometry.integer

import lambda.geometry.floating.FPoint

/**
  * A point with integer coordinates
  */
case class IPoint(x: Int, y: Int) {

  def toFPoint : FPoint = FPoint(x, y)
}
