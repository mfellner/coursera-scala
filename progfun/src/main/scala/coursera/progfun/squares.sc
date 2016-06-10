def sqrt(x: Double) = {
  def isGoodEnough(guess: Double) =
    Math.abs(guess * guess - x) / x < 0.001

  def improve(guess: Double) =
    (guess + x / guess) / 2.0

  def sqrtIter(guess: Double): Double = {
    if (isGoodEnough(guess))
      guess
    else
      sqrtIter(improve(guess))
  }

  sqrtIter(1.0)
}

sqrt(0.001)
sqrt(0.1e-20)
sqrt(1.6777216E7)

Math.sqrt(0.001)
Math.sqrt(0.1e-20)
Math.sqrt(1.6777216E7)
