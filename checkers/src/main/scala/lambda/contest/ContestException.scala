package lambda.contest

/**
  * @author Ilya Sergey
  */
case class ContestException(loc: String, data: Any = None) extends Exception {
  override def getMessage = s"Location: $loc\nMessage:\n${data.toString}"
}
