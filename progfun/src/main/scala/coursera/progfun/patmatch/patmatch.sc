import coursera.progfun.patmatch._

import scala.util.Random

Sum(Prod(Number(2), Var("x")), Var("y"))
Prod(Sum(Number(2), Var("x")), Var("y"))

val xs = Random.shuffle(1 to 10).toList
isort(xs)
