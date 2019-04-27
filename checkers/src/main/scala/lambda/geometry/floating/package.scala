package lambda.geometry

package object floating {

  case class DoubleEpsComparable(d: Double) extends EpsEq[DoubleEpsComparable] {
    def =~=(q: DoubleEpsComparable) = scala.math.abs(d - q.d) < Eps
    def <=~(q: DoubleEpsComparable) = this.d < q.d || this =~= q
    def >=~(q: DoubleEpsComparable) = this.d > q.d || this =~= q
    def <~(q: DoubleEpsComparable) = this.d < q.d && this =!= q
    def >~(q: DoubleEpsComparable) = this.d > q.d && this =!= q
  }

  implicit def _double2EpsComparable(d: Double): DoubleEpsComparable = DoubleEpsComparable(d)

  def seqEpsEq[T <: EpsEq[T]](s1: Seq[T], s2: Seq[T]): Boolean =
    s1.zip(s2).forall { case (v1, v2) => v1 =~= v2 }


  // Constants
  val PI = scala.math.Pi
  val quarter_pi = PI / 4
  val half_pi = PI / 2
  val PI2 = 2 * PI

  val Eps: Double = 0.000000001

  def normalize(d: Double, period: Double): Double = {
    var tmp = d
    while (tmp <~ 0.0) {
      tmp = tmp + period
    }
    while (tmp >= period) {
      tmp = tmp - period
    }
    tmp
  }

  /**
    * Turn an almost-Int into an actual Int
    */
  def roundToClosestInt(d: Double): Double = {
    val i = if (d > 0) (d + 2 * Eps).toInt
    else if (d < 0) (d - 2 * Eps).toInt else 0
    if (d =~= i.toDouble) i else d
  }

  type ToDouble = {def toDouble: Double}

  /**
    * Rotate the sequence s until e (such that e \in s) is not the first element.
    *
    * If e \notin s, returns unmodified sequence s and 0,
    * Otherwise the results the resulting sequence and a number of rotations made.
    */
  def rotateSeq[T <: EpsEq[T]](s: Seq[T], e: T): (Seq[T], Int) = {
    if (!s.contains(e)) return (s, 0)
    rotateSeqLoop(s, e, 0)
  }

  private def rotateSeqLoop[T <: EpsEq[T]](s: Seq[T], e: T, rNum: Int): (Seq[T], Int) = {
    if (s.head =~= e) (s, rNum) else rotateSeqLoop(s.tail ++ Seq(s.head), e, rNum + 1)
  }


  /**
    * Checks if d1 and d2 are equal modulo period per
    */
  def checkPeriod(d1: Double, d2: Double, per: Double): Boolean = {
    assert(per > 0, "Period should be positive.")
    if (d1 =~= d2) return true
    var tmp = d1
    if (d1 <~ d2) {
      while (tmp <~ d2) {
        tmp = tmp + per
      }
    } else {
      // d1 > d2
      while (tmp >~ d2) {
        tmp = tmp - per
      }
    }

    tmp =~= d2
  }


}
