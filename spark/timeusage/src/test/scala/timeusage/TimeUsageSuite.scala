package timeusage

import org.apache.spark.sql.{ColumnName, DataFrame, Row}
import org.apache.spark.sql.types.{
DoubleType,
StringType,
StructField,
StructType
}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterAll, FunSuite}

import scala.util.Random

@RunWith(classOf[JUnitRunner])
class TimeUsageSuite extends FunSuite with BeforeAndAfterAll {

  test("read data source") {
    val (headerColumns, dataFrame) = TimeUsage.read("/timeusage/atussum.csv")
    assert(headerColumns.length === 455)
    val head = dataFrame.head()
    assert(head.length === 455)
    assert(head(0).isInstanceOf[String])
    for (i <- 1 until head.length)
      assert(head(i).isInstanceOf[Double])
  }

  test("classifiedColumns") {
    val (headerColumns, _) = TimeUsage.read("/timeusage/atussum.csv")

    val (primary, working, other) = TimeUsage.classifiedColumns(headerColumns)
    assert(primary.nonEmpty)
    assert(working.nonEmpty)
    assert(other.nonEmpty)
    assert(primary.length + working.length + other.length === 455)
    assert(primary.toSet - working.toSet - other.toSet === primary.toSet)
    assert(working.toSet - primary.toSet - other.toSet === working.toSet)
    assert(other.toSet - primary.toSet - working.toSet === other.toSet)
  }

  test("timeUsageSummary") {
    val (headerColumns, df) = TimeUsage.read("/timeusage/atussum.csv")
    val (primary, working, other) = TimeUsage.classifiedColumns(headerColumns)

    val summaryDf = TimeUsage.timeUsageSummary(primary, working, other, df.sample(withReplacement = false, 0.1))
    assert(summaryDf.head() != null)
    summaryDf.show(10)
  }
}
