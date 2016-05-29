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
  def balance(chars: List[Char]): Boolean = ???

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = ???
}
