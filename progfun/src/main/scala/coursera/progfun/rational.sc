class Rational(x: Int, y: Int) {
  require(y != 0, "y must not be 0")

  def this(x: Int) = this(x, 1)

  private lazy val g = gcd(x, y)

  val numer = x

  val denom = y

  private def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)

  def add(that: Rational) =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom)

  def unary_- = new Rational(-numer, denom)

  def sub(that: Rational) = add(-that)

  def + = add _

  def - = sub _

  def less(that: Rational) =
    numer * that.denom < that.numer * denom

  def < = less _

  def >(that: Rational) = that < this

  def max(that: Rational) =
    if (this less that) that else this

  override def toString =
    numer / g + "/" + denom / g
}

val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)
x + y
x - y - z
x + y
x < y
x > y
x max y
new Rational(2)
