package coursera.progfun

package object mapfun {

  def isPrime(n: Int): Boolean =
    (2 until n).forall(d => n % d != 0)

  def scalarProduct(xs: List[Int], ys: List[Int]): Double = (for {
    (x, y) <- xs zip ys
  } yield x * y).sum


  def queens(n: Int): Set[List[Int]] = {
    def isSafe(col: Int, queens: List[Int]): Boolean = {
      val row = queens.length
      val queensWithRow = (row - 1 to 0 by -1) zip queens
      queensWithRow.forall {
        case (r, c) => col != c && math.abs(col - c) != row - r
      }
    }

    def placeQueens(k: Int): Set[List[Int]] = {
      if (k > 0)
        for {
          queens <- placeQueens(k - 1)
          col <- 0 until n
          if isSafe(col, queens)
        } yield col :: queens
      else Set(List())
    }
    placeQueens(n)
  }

  def show(queens: List[Int]) = {
    val lines = for {
      col <- queens.reverse
    } yield Vector.fill(queens.length)("* ").updated(col, "X ").mkString
    "\n" + (lines mkString "\n")
  }

  class Poly(terms0: Map[Int, Double]) {
    def this(bindings: (Int, Double)*) = this(bindings.toMap)

    val terms = terms0 withDefaultValue 0.0

    private def adjust(term: (Int, Double)): (Int, Double) = {
      val (exp, coeff) = term
      exp -> (coeff + terms(exp))
    }

    //    def +(other: Poly) = new Poly(terms ++ (other.terms map adjust))

    private def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = terms + adjust(term)

    def +(other: Poly) = new Poly((other.terms foldLeft terms) (addTerm))

    override def toString = (for {
      (exp, coeff) <- terms.toList.sorted.reverse
    } yield coeff + "x^" + exp) mkString " + "
  }

  def Poly(bindings: (Int, Double)*): Poly = {
    new Poly(bindings.toMap)
  }
}
