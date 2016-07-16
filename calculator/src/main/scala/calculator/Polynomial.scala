package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double], c: Signal[Double]): Signal[Double] = Signal {
    val bv = b()
    bv * bv - 4 * a() * c()
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = Signal {
    val dv = delta()
    val av = a()
    val bv = b()
    val cv = c()
    if (dv < 0) Set()
    else Set((-bv + math.sqrt(dv)) / 2 * av, (-bv - math.sqrt(dv)) / 2 * av)
  }
}
