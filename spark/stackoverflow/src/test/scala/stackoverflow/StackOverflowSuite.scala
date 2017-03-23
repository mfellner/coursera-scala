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
    val question1 = Posting(1, 101, Some(102), None, 10, Some("JavaScript"))
    val answer1 = Posting(2, 102, None, Some(101), 10, None)

    val postings = StackOverflow.sc.parallelize(Seq(
      question1,
      answer1
    ))

    val grouped = testObject.groupedPostings(postings).collect()

    assert(grouped.length == 1)
    assert(grouped(0)._1 == question1.id)
    assert(grouped(0)._2.size == 1)
    assert(grouped(0)._2.head._1 == question1, "Posting is not the question.")
    assert(grouped(0)._2.head._2 == answer1, "Posting is not the answer.")
  }

  test("scoredPostings") {
    val question1 = Posting(1, 101, Some(104), None, 10, Some("JavaScript"))
    val question2 = Posting(1, 102, Some(106), None, 10, Some("JavaScript"))

    val grouped: RDD[(Int, Iterable[(Posting, Posting)])] = StackOverflow.sc.parallelize(Seq(
      (question1.id, Iterable(
        (question1, Posting(2, 103, None, Some(question1.id), 10, None)),
        (question1, Posting(2, 104, None, Some(question1.id), 12, None)),
        (question1, Posting(2, 105, None, Some(question1.id), 11, None))
      )),
      (question2.id, Iterable(
        (question2, Posting(2, 105, None, Some(question2.id), 10, None)),
        (question2, Posting(2, 107, None, Some(question2.id), 13, None))
      ))
    ))

    val scored = testObject.scoredPostings(grouped).collect().toSeq.sortBy({ case (posting, _) => posting.id })

    assert(scored.length == 2)
    assert(scored.head._1 == question1)
    assert(scored.head._2 == 12)
    assert(scored(1)._1 == question2)
    assert(scored(1)._2 == 13)
  }
}
