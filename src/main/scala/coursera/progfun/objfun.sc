trait List[T] {
  def isEmpty: Boolean

  def head: T

  def tail: List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  override def isEmpty: Boolean = false

  override def toString: String = head.toString + "," + tail.toString
}

class Nil[T] extends List[T] {
  def isEmpty: Boolean = true

  def head = throw new NoSuchElementException

  def tail = throw new NoSuchElementException

  override def toString = ""
}

object List {
  def apply[T](): List[T] = new Nil

  def apply(x: Int) = new Cons(x, new Nil)

  def apply(x: Int, y: Int) = new Cons(x, new Cons(y, new Nil))
}

val l = List(1, 2)
