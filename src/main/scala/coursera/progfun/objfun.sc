trait List[+T] {
  def isEmpty: Boolean

  def head: T

  def tail: List[T]

  def prepend[U >: T](elem: U): List[U] = new Cons(elem, this)
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  override def isEmpty: Boolean = false

  override def toString: String = head.toString + "," + tail.toString
}

object Nil extends List[Nothing] {
  def isEmpty: Boolean = true

  def head = throw new NoSuchElementException

  def tail = throw new NoSuchElementException

  override def toString = ""
}

object List {
  def apply[T](): List[T] = Nil

  def apply(x: Int) = new Cons(x, Nil)

  def apply(x: Int, y: Int) = new Cons(x, new Cons(y, Nil))
}

val l = List(1, 2)
