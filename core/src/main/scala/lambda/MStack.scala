package lambda

/**
  * A simple mutable stack, implemented as a list wrapper
  *
  * @author Ilya Sergey
  */

class MStack[T] {

  private var stack : List[T] = Nil

  def this(l: List[T]) = {
    this()
    this.stack = l
  }

  def push(e : T): MStack[T] = {
    stack = e :: stack
    this
  }

  def pushAll(s: Seq[T]): MStack[T] = {
    s.foreach(e => this.push(e))
    this
  }

  def top : Option[T] = stack match {
    case h :: _ => Some(h)
    case _ => None
  }

  def pop(): Option[T] = stack match {
    case h :: t =>
      stack = t
      Some(h)
    case _ => None
  }

  def tail : Seq[T] = stack match {
    case _ :: t => t
    case _ => throw new NoSuchElementException
  }

  def toSeq: Seq[T] = stack.toSeq

}
