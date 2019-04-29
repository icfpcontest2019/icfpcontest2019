package lambda

/**
  * Generic utility functions
  *
  * @author Ilya Sergey
  */


package object geometry {

  /**
    * Rotate sequence s n times.
    */
  def rotateSeqNum[T](s: Seq[T], n: Int): Seq[T] = {
    if (s.length <= 1) return s
    var tmp = n
    while (tmp < 0) {
      tmp = tmp + s.size
    }
    while (tmp >= s.size) {
      tmp = tmp - s.size
    }
    rotateSeqNumLoop(s, tmp)
  }

  def getEdges[T](vs: Seq[T]): Seq[(T, T)] =
    if (vs.size <= 1) Nil
    else {
      val n = vs.size
      (for (i <- 1 until n) yield (vs(i - 1), vs(i))) ++ Seq((vs(n - 1), vs.head))
    }

  def getTriples[T](s: Seq[T]): Seq[(T, T, T)] = {
    if (s.size < 3) return Nil

    def walk(l: List[T]): List[(T, T, T)] = l match {
      case x :: y :: Nil => {
        val (a, b) = (s.head, s.tail.head)
        List((x, y, a), (y, a, b))
      }
      case x :: y :: z :: t =>
        (x, y, z) :: walk(y :: z :: t)
      case _ => Nil
    }

    walk(s.toList)
  }


  def rotateSeqNumLoop[T](s: Seq[T], tmp: Int): Seq[T] =
    if (tmp == 0) s else rotateSeqNumLoop(s.tail ++ Seq(s.head), tmp - 1)


  def randomRotation[T](s: Seq[T]): Seq[T] = {
    val n = s.size
    val rot = (n * math.random).toInt
    rotateSeqNum(s, rot)
  }

  def randomElement[T](s: Seq[T]): T = {
    assert(s.nonEmpty)
    randomRotation(s).head
  }

  def randomIntBetween(i1: Int, i2: Int): Int = {
    ((i2 - i1 + 1) * math.random).toInt + i1
  }

  case class GeometryException(loc: String, data: Any) extends Exception {
    override def getMessage = s"Location: $loc\nMessage:\n${data.toString}"
  }

}
