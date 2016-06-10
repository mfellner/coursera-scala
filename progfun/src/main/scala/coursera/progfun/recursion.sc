def factorial(n: Int): Int = {
  def recur(acc: Int, n: Int): Int = {
    if (n == 0)
      acc
    else
      recur(acc * n, n - 1)
  }
  recur(1, n)
}

factorial(6)
1 * 2 * 3 * 4 * 5 * 6
