import coursera.progfun.mapfun._

isPrime(8)
isPrime(17)

scalarProduct(List(1, 2), List(1, 2))

val qs = queens(8)
(qs take 3 map show) mkString "\n"

val p1 = Poly(1 -> 2.0, 3 -> 4.0, 5 -> 6.2)
val p2 = Poly(0 -> 3.0, 3 -> 7.0)

p1 + p2
