package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def balance(chars: Array[Char]): Boolean = {
    @tailrec
    def recur(chars: Array[Char], open: Int): Boolean = {
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

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {
    @tailrec
    def traverse(idx: Int, until: Int, open: Int, close: Int): (Int, Int) = {
      if (idx == until)
        (open, close)
      else {
        val diff = math.abs(open) - math.abs(close)
        val open2 = chars(idx) match {
          case '(' => open + 1
          case ')' => open - 1
          case _ => open
        }
        val close2 = chars(idx) match {
          case ')' => if (open2 < 0) close - 1 else close
          case _ => close
        }
        traverse(idx + 1, until, open2, close2)
      }
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from < threshold || threshold < 2) {
        val (open, close) = traverse(from, until, 0, 0)
        (open, close)
      } else {
        val mid = (from + until) / 2
        val ((open1, close1), (open2, close2)) = parallel(reduce(from, mid), reduce(mid, until))
        (open1 + open2, open1 + close2)
      }
    }

    reduce(0, chars.length) ==(0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
