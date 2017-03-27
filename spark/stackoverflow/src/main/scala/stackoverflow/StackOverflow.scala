package stackoverflow

import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.rdd.RDD
import annotation.tailrec
import scala.reflect.ClassTag
import org.apache.log4j.LogManager
import org.apache.log4j.Level


/** A raw stackoverflow posting, either a question or an answer */
case class Posting(postingType: Int,
                   id: Int,
                   acceptedAnswer: Option[Int],
                   parentId: Option[Int],
                   score: Int,
                   tags: Option[String]) extends Serializable {
  def isQuestion = postingType == 1

  def isAnswer = postingType == 2
}

object Posting {
  def newQuestion(id: Int, acceptedAnswer: Int, score: Int, tags: String) =
    Posting(1, id, Some(acceptedAnswer), None, score, Some(tags))

  def newAnswer(id: Int, parent: Int, score: Int) =
    Posting(2, id, None, Some(parent), score, None)
}


/** The main class */
object StackOverflow extends StackOverflow {

  @transient lazy val conf: SparkConf = new SparkConf().setMaster("local[3]").setAppName("StackOverflow")
  @transient lazy val sc: SparkContext = new SparkContext(conf)

  /** Main function */
  def main(args: Array[String]): Unit = {

    val lines = sc.textFile("src/main/resources/stackoverflow/stackoverflow.csv")
    val raw = rawPostings(lines)
    val grouped = groupedPostings(raw)
    val scored = scoredPostings(grouped)
    //.sample(withReplacement = true, 0.1, 0) // TODO: remove downsampling
    val vectors = vectorPostings(scored)
    //    assert(vectors.count() == 2121822, "Incorrect number of vectors: " + vectors.count())

    LogManager.getRootLogger.setLevel(Level.WARN)

    val means = kmeans(sampleVectors(vectors), vectors, debug = true)
    val results = clusterResults(means, vectors)
    printResults(results)
    //    println(means)
  }
}


/** The parsing and kmeans methods */
class StackOverflow extends Serializable {

  /** Languages */
  val langs = List(
    "JavaScript", "Java", "PHP", "Python", "C#", "C++", "Ruby", "CSS",
    "Objective-C", "Perl", "Scala", "Haskell", "MATLAB", "Clojure", "Groovy")

  /** K-means parameter: How "far apart" languages should be for the kmeans algorithm? */
  def langSpread = 50000

  assert(langSpread > 0, "If langSpread is zero we can't recover the language from the input data!")

  /** K-means parameter: Number of clusters */
  def kmeansKernels = 45

  /** K-means parameter: Convergence criteria */
  def kmeansEta: Double = 20.0D

  /** K-means parameter: Maximum iterations */
  def kmeansMaxIterations = 120


  //
  //
  // Parsing utilities:
  //
  //

  /** Load postings from the given file */
  def rawPostings(lines: RDD[String]): RDD[Posting] =
    lines.map(line => {
      val arr = line.split(",")
      Posting(postingType = arr(0).toInt,
        id = arr(1).toInt,
        acceptedAnswer = if (arr(2) == "") None else Some(arr(2).toInt),
        parentId = if (arr(3) == "") None else Some(arr(3).toInt),
        score = arr(4).toInt,
        tags = if (arr.length >= 6) Some(arr(5).intern()) else None)
    })


  /** Group the questions and answers together */
  def groupedPostings(postings: RDD[Posting]): RDD[(Int, Iterable[(Posting, Posting)])] = {
    val questions = postings.filter(_.isQuestion).keyBy(_.id)
    val answers = postings.filter(_.isAnswer).keyBy(_.parentId).flatMap {
      case (Some(id), answer) => Some(id, answer)
      case _ => None
    }
    questions.join(answers).groupByKey()
  }


  /** Compute the maximum score for each posting */
  def scoredPostings(grouped: RDD[(Int, Iterable[(Posting, Posting)])]): RDD[(Posting, Int)] = {

    def answerHighScore(as: Array[Posting]): Int = {
      var highScore = 0
      var i = 0
      while (i < as.length) {
        val score = as(i).score
        if (score > highScore)
          highScore = score
        i += 1
      }
      highScore
    }

    grouped.map {
      case (_, pairs) => (pairs.head._1, answerHighScore(pairs.map(_._2).toArray))
    }
  }


  /** Compute the vectors for the kmeans */
  def vectorPostings(scored: RDD[(Posting, Int)]): RDD[(Int, Int)] = {
    /** Return optional index of first language that occurs in `tags`. */
    def firstLangInTag(tag: Option[String], ls: List[String]): Option[Int] = {
      if (tag.isEmpty) None
      else if (ls.isEmpty) None
      else if (tag.get == ls.head) Some(0) // index: 0
      else {
        val tmp = firstLangInTag(tag, ls.tail)
        tmp match {
          case None => None
          case Some(i) => Some(i + 1) // index i in ls.tail => index i+1
        }
      }
    }

    val vectors = scored.flatMap {
      case (posting, score) => firstLangInTag(posting.tags, langs).map(index => (index * langSpread, score))
      case _ => None
    }

    vectors.cache()
  }


  /** Sample the vectors */
  def sampleVectors(vectors: RDD[(Int, Int)]): Array[(Int, Int)] = {

    assert(kmeansKernels % langs.length == 0, "kmeansKernels should be a multiple of the number of languages studied.")
    val perLang = kmeansKernels / langs.length

    // http://en.wikipedia.org/wiki/Reservoir_sampling
    def reservoirSampling(lang: Int, iter: Iterator[Int], size: Int): Array[Int] = {
      val res = new Array[Int](size)
      val rnd = new util.Random(lang)

      for (i <- 0 until size) {
        assert(iter.hasNext, s"iterator must have at least $size elements")
        res(i) = iter.next
      }

      var i = size.toLong
      while (iter.hasNext) {
        val elt = iter.next
        val j = math.abs(rnd.nextLong) % i
        if (j < size)
          res(j.toInt) = elt
        i += 1
      }

      res
    }

    val res =
      if (langSpread < 500)
      // sample the space regardless of the language
        vectors.takeSample(false, kmeansKernels, 42)
      else
      // sample the space uniformly from each language partition
        vectors.groupByKey.flatMap({
          case (lang, vectors) => reservoirSampling(lang, vectors.toIterator, perLang).map((lang, _))
        }).collect()

    assert(res.length == kmeansKernels, res.length)
    res
  }


  //
  //
  //  Kmeans method:
  //
  //

  def update(classified: RDD[((Int, Int), Iterable[(Int, Int)])], oldMeans: Array[(Int, Int)]): Array[(Int, Int)] = {
    //    oldMeans.map(oldMean => {
    //      val vectors = classified.filter(_ == oldMean).take(1) match {
    //        case Array(t) => t._2
    //        case _ => Iterable()
    //      }
    //      averageVectors(vectors)
    //    })
    oldMeans.map(oldMean => classified.filter(_._1 == oldMean).take(1) match {
      case Array(t) => averageVectors(t._2)
      case _ => oldMean
    })
  }


  /**
    * For each vector, find the mean closes to it.
    *
    * @return Map of each mean to the vectors closest to it.
    */
  def classify(vectors: RDD[(Int, Int)], means: Array[(Int, Int)]): RDD[((Int, Int), Iterable[(Int, Int)])] =
    vectors.groupBy(vector => means(findClosest(vector, means))).cache()

  /** Main kmeans computation */
  @tailrec final def kmeans(means: Array[(Int, Int)], vectors: RDD[(Int, Int)], iter: Int = 1, debug: Boolean = false): Array[(Int, Int)] = {
    //    val newMeans = update(classify(vectors, means), means)

    val newMeans = means.clone()
    val oldMeans2Vectors = vectors.map(p => (means(findClosest(p, means)), p)).groupByKey()
    val oldMeans2NewAverages = oldMeans2Vectors.mapValues(averageVectors).collectAsMap()

    for (i <- means.indices) {
      val oldMean = means(i)
      if (oldMeans2NewAverages.contains(oldMean)) {
        newMeans(i) = oldMeans2NewAverages(oldMean)
      }
    }

    //    val classified = vectors.groupBy(vector => means(findClosest(vector, means))).collect()

    //    classified.foreach()
    //    classified.foreach(entry => newMeans(entry._1) = entry._2)
    //    val newMeans = means.zipWithIndex.map {
    //      case (_, index) => averageVectors(classified(index)._2)
    //    }

    // TODO: Fill in the newMeans array
    val distance = euclideanDistance(means, newMeans)

    if (debug) {
      println(
        s"""Iteration: $iter
           |  * current distance: $distance
           |  * desired distance: $kmeansEta
           |  * means:""".stripMargin)
      for (idx <- 0 until kmeansKernels)
        println(f"   ${means(idx).toString}%20s ==> ${newMeans(idx).toString}%20s  " +
          f"  distance: ${euclideanDistance(means(idx), newMeans(idx))}%8.0f")
    }

    if (converged(distance))
      newMeans
    else if (iter < kmeansMaxIterations)
      kmeans(newMeans, vectors, iter + 1, debug)
    else {
      println("Reached max iterations!")
      newMeans
    }
  }


  //
  //
  //  Kmeans utilities:
  //
  //

  /** Decide whether the kmeans clustering converged */
  def converged(distance: Double) =
    distance < kmeansEta


  /** Return the euclidean distance between two points */
  def euclideanDistance(v1: (Int, Int), v2: (Int, Int)): Double = {
    val part1 = (v1._1 - v2._1).toDouble * (v1._1 - v2._1)
    val part2 = (v1._2 - v2._2).toDouble * (v1._2 - v2._2)
    part1 + part2
  }

  /** Return the euclidean distance between two points */
  def euclideanDistance(a1: Array[(Int, Int)], a2: Array[(Int, Int)]): Double = {
    assert(a1.length == a2.length)
    var sum = 0d
    var idx = 0
    while (idx < a1.length) {
      sum += euclideanDistance(a1(idx), a2(idx))
      idx += 1
    }
    sum
  }

  /**
    * Return the closest point.
    *
    * @return Index of the closest mean int the input array.
    */

  def findClosest(p: (Int, Int), centers: Array[(Int, Int)]): Int = {
    var bestIndex = 0
    var closest = Double.PositiveInfinity
    for (i <- 0 until centers.length) {
      val tempDist = euclideanDistance(p, centers(i))
      if (tempDist < closest) {
        closest = tempDist
        bestIndex = i
      }
    }
    bestIndex
  }


  /** Average the vectors */
  def averageVectors(ps: Iterable[(Int, Int)]): (Int, Int) = {
    val iter = ps.iterator
    var count = 0
    var comp1: Long = 0
    var comp2: Long = 0
    while (iter.hasNext) {
      val item = iter.next
      comp1 += item._1
      comp2 += item._2
      count += 1
    }
    ((comp1 / count).toInt, (comp2 / count).toInt)
  }


  //
  //
  //  Displaying results:
  //
  //
  def clusterResults(means: Array[(Int, Int)], vectors: RDD[(Int, Int)]): Array[(String, Double, Int, Int)] = {
    val closest = vectors.map(p => (findClosest(p, means), p))
    val closestGrouped = closest.groupByKey()

    val median = closestGrouped.mapValues { vs =>
      val langOccurences = vs.map(_._1).groupBy(x => x).mapValues(_.size)
      val langIndex = langOccurences.maxBy(_._2)._1
      val langLabel: String = langs(langIndex / langSpread)
      // most common language in the cluster
      val langPercent: Double = if (langOccurences.size == 1) 100F
      else langOccurences(langIndex) / (langOccurences - langIndex).values.sum.toDouble
      // percent of the questions in the most common language
      val vsArray = vs.toArray.sortBy(_._2)
      val clusterSize: Int = vsArray.length
      val isOdd: Boolean = vsArray.length % 2 != 0
      val medianScore: Int = if (vsArray.length == 1)
        vsArray.head._2
      else if (isOdd) {
        val i = (vsArray.length + 1) / 2
        vsArray(i)._2
      } else {
        val i1 = vsArray.length / 2
        val i2 = i1 - 1
        (vsArray(i1)._2 + vsArray(i2)._2) / 2
      }

      (langLabel, langPercent, clusterSize, medianScore)
    }

    median.collect().map(_._2).sortBy(_._4)
  }

  def printResults(results: Array[(String, Double, Int, Int)]): Unit = {
    println("Resulting clusters:")
    println("  Score  Dominant language (%percent)  Questions")
    println("================================================")
    for ((lang, percent, size, score) <- results)
      println(f"${
        score
      }%7d  ${
        lang
      }%-17s (${
        percent
      }%-5.1f%%)      ${
        size
      }%7d")
  }
}
