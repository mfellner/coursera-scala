package reductions

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._

import ParallelParenthesesBalancing._

@RunWith(classOf[JUnitRunner])
class ParallelParenthesesBalancingSuite extends FunSuite {

  test("balance should work for empty string") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance($input) should be $expected")

    check("", true)
  }

  test("balance should work for string of length 1") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance($input) should be $expected")

    check("(", false)
    check(")", false)
    check(".", true)
  }

  test("balance should work for string of length 2") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance($input) should be $expected")

    check("()", true)
    check(")(", false)
    check("((", false)
    check("))", false)
    check(".)", false)
    check(".(", false)
    check("(.", false)
    check(").", false)
  }

  // Parallel implementation

  test("parBalance should work for empty string") {
    def check(input: String, expected: Boolean) =
      assert(parBalance(input.toArray, 4) == expected,
        s"parBalance($input) should be $expected")

    check("", true)
  }

  test("parBalance should work for string of length 1") {
    def check(input: String, expected: Boolean) =
      assert(parBalance(input.toArray, 4) == expected,
        s"parBalance($input) should be $expected")

    check("(", false)
    check(")", false)
    check(".", true)
  }

  test("parBalance should work for strings of different lengths with threshold 2") {
    def check(input: String, threshold: Int, expected: Boolean) =
      assert(parBalance(input.toArray, threshold) == expected,
        s"parBalance($input) should be $expected")

    check(".)", 2, false)
    check(".(", 2, false)
    check("(.", 2, false)
    check(").", 2, false)
    check(")", 2, false)
    check("(", 2, false)
    check("((", 2, false)
    check("))", 2, false)
    check(")(", 2, false)
    check("(((", 2, false)
    check(")))", 2, false)
    check("))))", 2, false)
    check("((((", 2, false)
    check(")()(", 2, false)
    check(")()()()(", 2, false)
    check(")(())(", 2, false)
    check(")((", 2, false)
    check("))(", 2, false)
    check("()(", 2, false)
    check(")()", 2, false)

    check("()", 2, true)
    check("()()", 2, true)
    check("()()()", 2, true)
    check("()()()()", 2, true)
    check("(())", 2, true)
    check("((()))", 2, true)
    check("(((())))", 2, true)
    check("(())()", 2, true)
    check("()(())", 2, true)
    check("()(())()(())", 2, true)
  }

  test("parBalance should work for strings of different lengths with threshold 3") {
    def check(input: String, threshold: Int, expected: Boolean) =
      assert(parBalance(input.toArray, threshold) == expected,
        s"parBalance($input) should be $expected")

    check("((", 3, false)
    check("))", 3, false)
    check(")(", 3, false)
    check("(((", 3, false)
    check(")))", 3, false)
    check("))))", 3, false)
    check("((((", 3, false)
    check(")()(", 3, false)
    check(")()()()(", 3, false)
    check(")(())(", 3, false)
    check(")((", 3, false)
    check("))(", 3, false)
    check("()(", 3, false)
    check(")()", 3, false)

    check("()", 3, true)
    check("()()", 3, true)
    check("()()()", 3, true)
    check("()()()()", 3, true)
    check("(())", 3, true)
    check("((()))", 3, true)
    check("(((())))", 3, true)
    check("(())()", 3, true)
    check("()(())", 3, true)
    check("()(())()(())", 3, true)
    check("()((foo))(bar)((.))", 3, true)
  }

  test("parBalance should invoke the parallel construct 7 times for string '()()()()' and threshold 1"){
    def check(input: String, threshold: Int, expected: Boolean) =
      assert(parBalance(input.toArray, threshold) == expected,
        s"parBalance($input) should be $expected")

    check("()()()()", 1, true)
  }
}
