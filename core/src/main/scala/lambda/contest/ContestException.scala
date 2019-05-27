package lambda.contest

import lambda.geometry.integer.IPoint

/**
  * @author Ilya Sergey
  */
case class ContestException(msg: String, loc: Option[IPoint] = None) extends Exception {
  override def getMessage = msg
}
