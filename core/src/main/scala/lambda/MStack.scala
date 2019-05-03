package lambda

/**
  * A simple mutable stack, implemented as a list wrapper
  *
  * @author Ilya Sergey
  */

class MStack[T] extends Seq[T] {

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

  override def tail : Seq[T] = stack match {
    case _ :: t => t
    case _ => throw new NoSuchElementException
  }

  override def toSeq: Seq[T] = stack.toSeq

  override def length = stack.length

  override def apply(idx: Int) = stack(idx)

  override def iterator = stack.iterator
}
