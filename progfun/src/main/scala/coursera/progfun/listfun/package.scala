package coursera.progfun

import scala.math.Ordering

package object listfun {
  def removeAt[T](n: Int, xs: List[T]): List[T] = {
    def recur(i: Int, ys: List[T]): List[T] = ys match {
      case List() => xs
      case y :: rest => if (i == n) rest else y :: recur(i + 1, rest)
    }
    recur(0, xs)
  }

  def removeAt2[T](n: Int, xs: List[T]): List[T] = for {
    (x, i) <- xs.zipWithIndex
    if i != n
  } yield x

  def removeAt3[T](n: Int, xs: List[T]): List[T] =
    (xs take n) ::: (xs drop n + 1)

  def flatten(xs: List[Any]): List[Any] = xs match {
    case (y: List[Any]) :: ys => flatten(y) ::: flatten(ys)
    case (y: Any) :: ys => y :: flatten(ys)
    case _ => xs
  }

  def flatten2(xs: List[Any]): List[Any] = xs.flatMap {
    case ys: Seq[Any] => flatten2(ys.toList)
    case ys: Any => List(ys)
  }

  def mergesort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
    def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
      case (_, Nil) => xs
      case (Nil, _) => ys
      case (x :: xrest, y :: yrest) =>
        if (ord.lt(x, y)) x :: merge(xrest, ys)
        else y :: merge(xs, yrest)
    }

    val n = xs.length / 2
    if (n == 0) xs
    else {
      val (as, bs) = xs splitAt n
      merge(mergesort(as), mergesort(bs))
    }
  }

  def squareList1(xs: List[Int]): List[Int] =
    xs match {
      case Nil => xs
      case y :: ys => (y * y) :: squareList1(ys)
    }

  def squareList2(xs: List[Int]): List[Int] =
    xs map (math.pow(_, 2).toInt)

  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case x :: xs1 =>
      val (xs2, rest) = xs.span(_ == x)
      xs2 :: pack(rest)
  }

  def encode[T](xs: List[T]): List[(T, Int)] =
    pack(xs) map (ys => (ys.head, ys.length))

  def mapFun[T, U](xs: List[T], f: T => U): List[U] =
    (xs foldRight List[U]()) (f(_) :: _)

  def lengthFun[T](xs: List[T]): Int =
    (xs foldRight 0) ((_, n) => n + 1)
}
