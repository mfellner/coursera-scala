package stackoverflow

import org.scalatest.{BeforeAndAfterAll, FunSuite}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.rdd.RDD
import java.io.File

import org.apache.log4j.{Level, LogManager}

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

  LogManager.getRootLogger.setLevel(Level.WARN)

  lazy val question1: Posting = Posting.newQuestion(id = 10, acceptedAnswer = 13, score = 100, "JavaScript")
  lazy val question2: Posting = Posting.newQuestion(id = 11, acceptedAnswer = 15, score = 110, "Scala")
  lazy val question3: Posting = Posting.newQuestion(id = 12, acceptedAnswer = 17, score = 120, "Java")
  lazy val answer1: Posting = Posting.newAnswer(id = 13, parent = question1.id, score = 101)
  lazy val answer2: Posting = Posting.newAnswer(id = 14, parent = question1.id, score = 111)
  lazy val answer3: Posting = Posting.newAnswer(id = 15, parent = question2.id, score = 112)
  lazy val answer4: Posting = Posting.newAnswer(id = 16, parent = question2.id, score = 103)
  lazy val answer5: Posting = Posting.newAnswer(id = 17, parent = question3.id, score = 104)
  lazy val answer6: Posting = Posting.newAnswer(id = 18, parent = question3.id, score = 115)

  lazy val postings: RDD[Posting] = StackOverflow.sc.parallelize(Seq(
    question1, question2, question3,
    answer1, answer2, answer3, answer4, answer5, answer6
  ))

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
    val grouped = testObject.groupedPostings(postings).collect()

    assert(grouped.length == 3)

    for (group <- grouped) {
      assert(Set(10, 11, 12).contains(group._1))
      for (pair <- group._2) {
        assert(pair._1.isQuestion)
        assert(pair._2.isAnswer)
        assert(pair._1.id === pair._2.parentId.get)
      }
    }
  }

  test("scoredPostings") {
    val grouped = testObject.groupedPostings(postings)
    val scored = testObject.scoredPostings(grouped).collect().toSeq.sortBy({ case (posting, _) => posting.id })

    assert(scored.length == 3)
    assert(scored(0)._1 == question1)
    assert(scored(1)._1 == question2)
    assert(scored(2)._1 == question3)
    assert(scored(0)._2 == 111)
    assert(scored(1)._2 == 112)
    assert(scored(2)._2 == 115)
  }

  test("vectorPostings") {
    val scored = testObject.scoredPostings(testObject.groupedPostings(postings))
    val vectors = testObject.vectorPostings(scored).collect().toSeq.sortBy({ case (_, score) => score })

    assert(vectors.length == 3)
    assert(vectors(0)._2 == 111)
    assert(vectors(1)._2 == 112)
    assert(vectors(2)._2 == 115)
    assert(vectors(0)._1 == testObject.langs.indexOf("JavaScript") * testObject.langSpread)
    assert(vectors(1)._1 == testObject.langs.indexOf("Scala") * testObject.langSpread)
    assert(vectors(2)._1 == testObject.langs.indexOf("Java") * testObject.langSpread)
  }
}
