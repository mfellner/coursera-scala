abstract class Nat {
  def isZero: Boolean

  def predecessor: Nat

  def successor: Nat = new Succ(this)

  def +(that: Nat): Nat

  def -(that: Nat): Nat

  def toStringHelper(x: Int): Int

  override def toString: String = toStringHelper(0).toString
}

object Zero extends Nat {
  override def isZero: Boolean = true

  override def +(that: Nat): Nat = that

  override def -(that: Nat) = throw new UnsupportedOperationException

  override def predecessor = throw new NoSuchElementException

  override def toStringHelper(x: Int): Int = x
}

class Succ(n: Nat) extends Nat {
  override def isZero: Boolean = false

  override def +(that: Nat): Nat = {
    def recur(a: Nat, b: Nat): Nat = {
      if (b.isZero) a
      else recur(new Succ(a), b.predecessor)
    }
    recur(this, that)
  }

  override def -(that: Nat): Nat = {
    def recur(a: Nat, b: Nat): Nat = {
      if (b.isZero) a
      else recur(a.predecessor, b.predecessor)
    }
    recur(this, that)
  }

  override def predecessor: Nat = n

  override def toStringHelper(x: Int): Int = predecessor.toStringHelper(x + 1)
}

Zero + new Succ(new Succ(Zero)) +
  Zero + new Succ(new Succ(Zero)) -
  new Succ(Zero) - Zero
