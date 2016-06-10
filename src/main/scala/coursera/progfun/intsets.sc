import coursera.progfun._

val t1 = new NonEmpty(3, Empty, Empty)
val t2 = t1 incl 4
val t3 = t2 incl 2

val t4 = new NonEmpty(3) incl 4 incl 1
val t5 = new NonEmpty(5) incl 2
t4 union t5
