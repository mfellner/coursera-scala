class Rational(x: Int, y: Int) {
  val numer = x

  val denom = y

  def add(that: Rational) =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom)

  def neg() =
    new Rational(-numer, denom)

  def sub(that: Rational) =
    add(that.neg())

  def + = add _

  def - = sub _

  override def toString =
    numer + "/" + denom
}

val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)
println(x + y)
println(x - y - z)
