package stackoverflow

import org.scalatest.{FunSuite, BeforeAndAfterAll}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.rdd.RDD
import java.io.File

@RunWith(classOf[JUnitRunner])
class StackOverflowSuite extends FunSuite with BeforeAndAfterAll {


  lazy val testObject = new StackOverflow {
    override val langs =
      List(
        "JavaScript", "Java", "PHP", "Python", "C#", "C++", "Ruby", "CSS",
        "Objective-C", "Perl", "Scala", "Haskell", "MATLAB", "Clojure", "Groovy")

    override def langSpread = 50000

    override def kmeansKernels = 45

    override def kmeansEta: Double = 20.0D

    override def kmeansMaxIterations = 120
  }

  test("testObject can be instantiated") {
    val instantiatable = try {
      testObject
      true
    } catch {
      case _: Throwable => false
    }
    assert(instantiatable, "Can't instantiate a StackOverflow object")
  }

  test("groupedPostings") {
    val postings = StackOverflow.sc.parallelize(Seq(
      Posting(1, 101, Some(102), None, 10, Some("JavaScript")),
      Posting(2, 102, None, Some(101), 10, None)
    ))

    val grouped = testObject.groupedPostings(postings).collect()

    assert(grouped.length == 1)
    assert(grouped(0)._1 == 101)
    assert(grouped(0)._2.size == 1)
    assert(grouped(0)._2.head._1.id == 101, "Posting is not the question.")
    assert(grouped(0)._2.head._2.id == 102, "Posting is not the answer.")
  }
}
