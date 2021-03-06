package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
  * This class is a test suite for the methods in object FunSets. To run
  * the test suite, you can either:
  * - run the "test" command in the SBT console
  * - right-click the file in eclipse and chose "Run As" - "JUnit Test"
  */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
    * Link to the scaladoc - very clear and detailed tutorial of FunSuite
    *
    * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
    *
    * Operators
    * - test
    * - ignore
    * - pending
    */

  /**
    * Tests are written using the "test" operator and the "assert" method.
    */
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
    * For ScalaTest tests, there exists a special equality operator "===" that
    * can be used inside "assert". If the assertion fails, the two values will
    * be printed in the error message. Otherwise, when using "==", the test
    * error message will only say "assertion failed", without showing the values.
    *
    * Try it out! Change the values so that the assertion fails, and look at the
    * error message.
    */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
    * When writing tests, one would often like to re-use certain values for multiple
    * tests. For instance, we would like to create an Int-set and have multiple test
    * about it.
    *
    * Instead of copy-pasting the code for creating the set into every test, we can
    * store it in the test class using a val:
    *
    * val s1 = singletonSet(1)
    *
    * However, what happens if the method "singletonSet" has a bug and crashes? Then
    * the test methods are not even executed, because creating an instance of the
    * test class fails!
    *
    * Therefore, we put the shared values into a separate trait (traits are like
    * abstract classes), and create an instance inside each test method.
    *
    */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s4 = singletonSet(4)
    val s5 = singletonSet(5)
  }

  /**
    * This test is currently disabled (by using "ignore") because the method
    * "singletonSet" is not yet implemented and the test would fail.
    *
    * Once you finish your implementation of "singletonSet", exchange the
    * function "ignore" by "test".
    */
  test("singletonSet(1) contains 1") {

    /**
      * We create a new instance of the "TestSets" trait, this gives us access
      * to the values "s1" to "s3".
      */
    new TestSets {
      /**
        * The string argument of "assert" is a message that is printed in case
        * the test fails. This helps identifying which assertion failed.
        */
      assert(contains(s1, 1), "Singleton")
      assert(contains(s2, 2), "Singleton")
      assert(contains(s3, 3), "Singleton")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect contains the common elements of two sets") {
    new TestSets {
      val su1 = union(s1, s2)
      val su2 = union(s2, s3)
      val s = intersect(su1, su2)
      assert(!contains(s, 1), "Intersect 1")
      assert(contains(s, 2), "Intersect 2")
      assert(!contains(s, 3), "Intersect 3")
    }
  }

  test("diff contains the elements of the first set that are not in the second set") {
    new TestSets {
      val su1 = union(s1, s2)
      val sd1 = diff(su1, s1)
      val sd2 = diff(su1, s2)
      val sd3 = diff(su1, su1)
      assert(contains(sd1, 2), "Diff 1")
      assert(!contains(sd1, 1), "Diff 1")

      assert(contains(sd2, 1), "Diff 2")
      assert(!contains(sd2, 2), "Diff 2")

      assert(!contains(sd3, 1), "Diff 2")
      assert(!contains(sd3, 2), "Diff 2")
    }
  }

  test("filter contains the elements that match the predicate") {
    new TestSets {
      val su = union(s1, union(s2, s3))
      val s = filter(su, x => x > 2)
      assert(!contains(s, 1), "Filter 1")
      assert(!contains(s, 2), "Filter 2")
      assert(contains(s, 3), "Filter 3")
    }
  }

  test("forall returns true if all elements in a set match the predicate") {
    new TestSets {
      val s = union(s1, union(s2, union(s3, union(s4, s5))))
      assert(forall(s, x => x < 6), "forall [1-5] < 6")
      assert(forall(s, x => x > 0), "forall [1-5] > 0")
      assert(!forall(s, x => x < 4), "forall [1-5] < 4")

      val sMinMax = union(singletonSet(-bound), union(s1, singletonSet(bound)))
      assert(forall(sMinMax, x => x >= -bound), "forall >= -bound")
      assert(forall(sMinMax, x => x <= bound), "forall <= +bound")
    }
  }

  test("exists returns true if at least one element in a set matches the predicate") {
    new TestSets {
      val s = union(s1, union(s2, union(s3, union(s4, s5))))
      assert(exists(s, x => x > 4), "exists [1-5] > 4")
      assert(exists(s, x => x < 2), "exists [1-5] < 2")
      assert(!exists(s, x => x > 5), "exists [1-5] > 5")

      val sMinMax = union(singletonSet(-bound), union(s1, singletonSet(bound)))
      assert(exists(sMinMax, x => x >= -bound), "exists >= -bound")
      assert(exists(sMinMax, x => x <= bound), "exists <= +bound")
    }
  }

  test("map applies a function to each element of a set and returns a new set") {
    new TestSets {
      val s = union(s1, union(s2, s3))
      assert(FunSets.toString(s) == "{1,2,3}", "map {1,2,3}")
      assert(FunSets.toString(map(s, x => x + 1)) == "{2,3,4}", "map {2,3,4}")
    }
  }
}
