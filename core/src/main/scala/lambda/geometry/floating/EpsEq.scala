package lambda.geometry.floating

/**
  * A trait for comparing values using epsilon-equality
  * @tparam T self type
  */
trait EpsEq[T <: EpsEq[T]] {
  def =~=(q: T): Boolean

  def =!=(q: T): Boolean = !(this =~= q)
}

