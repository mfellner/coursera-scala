package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    def recur(xs: Vector[Int], k: Int): Int = {
      if (c == k - 1)
        xs.last
      else
        recur(xs :+ (xs.last * ((r + 1 - k) / k.toDouble)).toInt, k + 1)
    }
    recur(Vector(1), 1)
  }

  def pascal2(c: Int, r: Int): Int = {
    def recur(xs: Vector[Int]): Vector[Int] = {
      if (r == xs.length - 1)
        xs
      else {
        val ys = (for {
          i <- xs.indices.dropRight(1)
        } yield xs(i) + xs(i + 1)).toVector
        recur(1 +: ys :+ 1)
      }
    }
    recur(Vector(1))(c)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def recur(chars: List[Char], open: Int): Boolean = {
      if (open < 0)
        false
      else if (chars.isEmpty)
        open == 0
      else {
        val x = chars.head match {
          case '(' => 1
          case ')' => -1
          case _ => 0
        }
        recur(chars.tail, open + x)
      }
    }
    recur(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def recur(m: Int, xs: List[Int]): Int = {
      if (xs.isEmpty)
        if (m == 0) 1 else 0
      else if (xs.head > m)
        recur(m, xs.tail)
      else
        recur(m, xs.tail) + recur(m - xs.head, xs)
    }

    if (money <= 0 || coins.isEmpty)
      0
    else
      recur(money, coins.sorted(Ordering[Int].reverse).dropWhile(_ > money))
  }
}
