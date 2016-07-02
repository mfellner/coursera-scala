package quickcheck

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

import scala.util.control.NonFatal

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeaps: Gen[H] = for {
    x <- arbitrary[A]
    h <- oneOf(const(empty), genHeap)
  } yield insert(x, h)

  lazy val genHeap: Gen[H] = for {
    h <- oneOf(const(empty), genHeaps)
  } yield h

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("empty heap must be empty") = forAll { (h: H) =>
    if (isEmpty(h)) h == empty else h != empty
  }

  property("empty heap retains min") = forAll { (h: H) =>
    if (isEmpty(h))
      h == empty
    else {
      val h2 = deleteMin(h)
      if (isEmpty(h2))
        h != h2 && h2 == empty
      else
        h != h2 && h2 != empty
    }
  }

  property("insert changes min") = forAll { (h1: H) =>
    val x = 2
    val h2 = insert(x, h1)
    if (isEmpty(h1)) {
      h1 != h2 && h2 != empty && findMin(h2) == x
    } else {
      val m1 = findMin(h1)
      val m2 = findMin(h2)
      if (m1 >= x)
        m2 == x
      else
        m2 < x
    }
  }

  property("insert two values changes min") = forAll { (h1: H) =>
    val x1 = 1
    val x2 = 2
    val h2 = insert(x2, insert(x1, h1))
    if (isEmpty(h1)) {
      findMin(h2) == x1
    } else {
      val m1 = findMin(h1)
      val m2 = findMin(h2)
      if (m1 >= x1)
        m2 == x1
      else
        m2 < x1
    }
  }

  property("insert into empty then delete should be empty") = forAll { (h1: H) =>
    val h2 = deleteMin(insert(3, h1))
    if (isEmpty(h1)) {
      isEmpty(h2) && h2 == empty && h1 == empty && h2 == h1
    } else {
      if (findMin(h1) > 3)
        findMin(h2) > 3
      else if (findMin(h1) == 3)
        findMin(h2) != 3
      else
        findMin(h2) <= 3
    }
  }

  property("melded heap must keep min") = forAll { (h1: H, h2: H) =>
    val h3 = meld(h1, h2)
    if (isEmpty(h1) && isEmpty(h2))
      isEmpty(h3) && h3 == empty && h3 == h2 && h3 == h1
    else if (isEmpty(h1) && !isEmpty(h2))
      !isEmpty(h3) && findMin(h3) == findMin(h2)
    else if (!isEmpty(h1) && isEmpty(h2))
      !isEmpty(h3) && findMin(h3) == findMin(h1)
    else {
      val m1 = findMin(h1)
      val m2 = findMin(h2)
      !isEmpty(h3) && findMin(h3) == math.min(m1, m2)
    }
  }

  property("heap min should be ordered") = forAll { (h: H) =>
    def recur(hn: H, m: A): Boolean = {
      if (isEmpty(hn))
        hn == empty
      else {
        if (findMin(hn) < m) false
        else findMin(hn) >= m && recur(deleteMin(hn), findMin(hn))
      }
    }
    if (isEmpty(h)) {
      val ms = List(1, 0, 3, 2)
      val hn = ms.foldLeft(h)((hx, x) => insert(x, hx))
      val res = ms.sorted.foldLeft((true, hn))((t, x) => (findMin(t._2) == x, deleteMin(t._2)))
      recur(hn, 0) && res._1
    } else {
      recur(h, Int.MinValue)
    }
  }

  property("delete min should remove value") = forAll { (h: H) =>
    if (isEmpty(h)) h == empty
    else {
      def clear(hn: H, xs: List[A]): (H, List[A]) = {
        if (isEmpty(hn)) (hn, xs)
        else clear(deleteMin(hn), findMin(hn) :: xs)
      }
      val (hc1, xs1) = clear(h, Nil)
      val (hc2, xs2) = clear(hc1, Nil)
      var err = false
      try {
        findMin(hc1)
        deleteMin(hc1)
        findMin(hc2)
        deleteMin(hc2)
      } catch {
        case e: Throwable => err = true
      }
      isEmpty(hc1) && isEmpty(hc2) && xs1.nonEmpty && xs2.isEmpty && err
    }
  }

  property("delete min should remove value (2)") = forAll { (xs: Set[A]) =>
    val h = xs.foldLeft(empty)((hx, x) => insert(x, hx))

    def recur(hn: H, xs: List[A]): Boolean = {
      if (isEmpty(hn)) xs.nonEmpty
      else if (xs.nonEmpty && xs.contains(findMin(hn))) false
      else recur(deleteMin(hn), findMin(hn) :: xs)
    }

    if (xs.isEmpty) isEmpty(h)
    else !isEmpty(h) && recur(h, Nil)
  }
}
