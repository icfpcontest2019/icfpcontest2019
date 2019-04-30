package lambda.geometry

/**
  * @author Ilya Sergey
  */
case class GeometryException(loc: String, data: Any) extends Exception {
  override def getMessage = s"Location: $loc\nMessage:\n${data.toString}"
}
