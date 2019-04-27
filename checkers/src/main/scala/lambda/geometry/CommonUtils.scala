package lambda.geometry

import java.util.concurrent._

/**
  * Generic utility functions
  *
  * @author Ilya Sergey
  */


/**
  * A trait for comparing values using epsilon-equality
  * @tparam T self type
  */
trait EpsEq[T <: EpsEq[T]] {
  def =~=(q: T): Boolean

  def =!=(q: T): Boolean = !(this =~= q)
}

object CommonUtils {

  def seqEpsEq[T <: EpsEq[T]](s1: Seq[T], s2: Seq[T]): Boolean =
    s1.zip(s2).forall { case (v1, v2) => v1 =~= v2 }


  // Constants
  val PI = scala.math.Pi
  val PI2 = 2 * PI
  val PI_12 = PI / 2
  val Eps: Double = 0.000000001

  /**
    */
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

  //  if (d > 0) {
  //    val i = if (d > 0) (d + 2 * Eps).toInt
  //    if (tmp =~= d) return
  //  }
  //  else if (d < 0) (d - 2 * Eps).toInt
  //  else 0

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

  def rotateSeqNumLoop[T](s: Seq[T], tmp: Int): Seq[T] =
    if (tmp == 0) s else rotateSeqNumLoop(s.tail ++ Seq(s.head), tmp - 1)

  case class DoubleEpsComparable(d: Double) extends EpsEq[DoubleEpsComparable] {
    def =~=(q: DoubleEpsComparable) = scala.math.abs(d - q.d) < Eps
    def <=~(q: DoubleEpsComparable) = this.d < q.d || this =~= q
    def >=~(q: DoubleEpsComparable) = this.d > q.d || this =~= q
    def <~(q: DoubleEpsComparable) = this.d < q.d && this =!= q
    def >~(q: DoubleEpsComparable) = this.d > q.d && this =!= q
  }

  implicit def _double2EpsComparable(d: Double): DoubleEpsComparable = DoubleEpsComparable(d)

  /**
    * Prevent the function from executing too much time
    *
    * @param time Time in millisecond
    * @param aux additional value to put in exception
    */
  def runWithTimer[T](task: () => T, time: Int, loc: String, aux: Any): T = {

    val executor: ExecutorService = Executors.newSingleThreadExecutor()
    val future = executor.submit(new Callable[T] {
      override def call() = task()
    })
    try {
      // retrurn result
      future.get(time, TimeUnit.MILLISECONDS)
    } catch {
      case e: TimeoutException =>
        future.cancel(true)
        throw GeometryException(loc, aux)
    }
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

  def randomRotation[T](s: Seq[T]): Seq[T] = {
    val n = s.size
    val rot = (n * math.random).toInt
    rotateSeqNum(s, rot)
  }

  def randomElement[T](s: Seq[T]) : T = {
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
