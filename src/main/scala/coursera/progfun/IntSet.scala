package coursera.progfun

abstract class IntSet {
  def contains(x: Int): Boolean

  def incl(x: Int): IntSet

  def union(other: IntSet): IntSet
}

object Empty extends IntSet {
  def contains(x: Int) = false

  def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)

  override def union(other: IntSet): IntSet = other

  override def toString = "."
}

class NonEmpty(elem: Int, left: IntSet = Empty, right: IntSet = Empty) extends IntSet {
  override def contains(x: Int): Boolean =
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true

  override def incl(x: Int): IntSet =
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this

  override def union(other: IntSet): IntSet =
    ((left union right) union other) incl elem

  override def toString = "{" + left + elem + right + "}"
}
