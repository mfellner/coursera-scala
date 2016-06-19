import scala.util.Random
import coursera.progfun.listfun._

removeAt(1, List('a', 'b', 'c', 'd'))
removeAt2(1, List('a', 'b', 'c', 'd'))
removeAt3(1, List('a', 'b', 'c', 'd'))

removeAt(2, List('a', 'b', 'c', 'd'))
removeAt2(2, List('a', 'b', 'c', 'd'))
removeAt3(2, List('a', 'b', 'c', 'd'))

flatten(List(List(1, 1), 2, List(3, List(5, 8))))
flatten2(List(List(1, 1), 2, List(3, List(5, 8))))

val xs = Random.shuffle((1 to 10).toList)
val ys = Random.shuffle(('a' to 'g').toList)

mergesort(xs)
mergesort(ys)

squareList1(List(1, 2, 3))
squareList2(List(1, 2, 3))

pack(List("a", "a", "a", "b", "c", "c", "a"))
encode(List("a", "a", "a", "b", "c", "c", "a"))

mapFun(List(1, 2, 3), (x: Int) => x * x)
lengthFun(List(1, 2, 3))
