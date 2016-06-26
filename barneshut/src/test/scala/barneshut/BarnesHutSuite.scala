package barneshut

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import scala.math._
import scala.collection.parallel._
import barneshut.conctrees.ConcBuffer

@RunWith(classOf[JUnitRunner])
class BarnesHutSuite extends FunSuite {

  // test cases for quad tree

  import FloatOps._

  test("Empty: center of mass should be the center of the cell") {
    val quad = Empty(51f, 46.3f, 5f)
    assert(quad.massX == 51f, s"${quad.massX} should be 51f")
    assert(quad.massY == 46.3f, s"${quad.massY} should be 46.3f")
  }

  test("Empty: mass should be 0") {
    val quad = Empty(51f, 46.3f, 5f)
    assert(quad.mass == 0f, s"${quad.mass} should be 0f")
  }

  test("Empty: total should be 0") {
    val quad = Empty(51f, 46.3f, 5f)
    assert(quad.total == 0, s"${quad.total} should be 0")
  }

  test("Leaf with 1 body") {
    val b = new Body(123f, 18f, 26f, 0f, 0f)
    val quad = Leaf(17.5f, 27.5f, 5f, Seq(b))

    assert(quad.mass ~= 123f, s"${quad.mass} should be 123f")
    assert(quad.massX ~= 18f, s"${quad.massX} should be 18f")
    assert(quad.massY ~= 26f, s"${quad.massY} should be 26f")
    assert(quad.total == 1, s"${quad.total} should be 1")
  }


  test("Fork with 3 empty quadrants and 1 leaf (nw)") {
    val b = new Body(123f, 18f, 26f, 0f, 0f)
    val nw = Leaf(17.5f, 27.5f, 5f, Seq(b))
    val ne = Empty(22.5f, 27.5f, 5f)
    val sw = Empty(17.5f, 32.5f, 5f)
    val se = Empty(22.5f, 32.5f, 5f)
    val quad = Fork(nw, ne, sw, se)

    assert(quad.centerX == 20f, s"${quad.centerX} should be 20f")
    assert(quad.centerY == 30f, s"${quad.centerY} should be 30f")
    assert(quad.mass ~= 123f, s"${quad.mass} should be 123f")
    assert(quad.massX ~= 18f, s"${quad.massX} should be 18f")
    assert(quad.massY ~= 26f, s"${quad.massY} should be 26f")
    assert(quad.total == 1, s"${quad.total} should be 1")
  }

  test("Empty.insert(b) should return a Leaf with only that body") {
    val quad = Empty(51f, 46.3f, 5f)
    val b = new Body(3f, 54f, 46f, 0f, 0f)
    val inserted = quad.insert(b)
    inserted match {
      case Leaf(centerX, centerY, size, bodies) =>
        assert(centerX == 51f, s"$centerX should be 51f")
        assert(centerY == 46.3f, s"$centerY should be 46.3f")
        assert(size == 5f, s"$size should be 5f")
        assert(bodies == Seq(b), s"$bodies should contain only the inserted body")
      case _ =>
        fail("Empty.insert() should have returned a Leaf, was $inserted")
    }
  }

  // test cases for Body

  test("Body.updated should do nothing for Empty quad trees") {
    val b1 = new Body(123f, 18f, 26f, 0f, 0f)
    val body = b1.updated(Empty(50f, 60f, 5f))

    assert(body.xspeed == 0f)
    assert(body.yspeed == 0f)
  }

  test("Body.updated should take bodies in a Leaf into account") {
    val b1 = new Body(123f, 18f, 26f, 0f, 0f)
    val b2 = new Body(524.5f, 24.5f, 25.5f, 0f, 0f)
    val b3 = new Body(245f, 22.4f, 41f, 0f, 0f)

    val quad = Leaf(15f, 30f, 20f, Seq(b2, b3))

    val body = b1.updated(quad)

    assert(body.xspeed ~= 12.587037f)
    assert(body.yspeed ~= 0.015557117f)
  }

  // test cases for sector matrix

  test("'SectorMatrix.+=' should add a body at (25,47) to the correct bucket of a sector matrix of size 96") {
    val body = new Body(5, 25, 47, 0.1f, 0.1f)
    val boundaries = new Boundaries()
    boundaries.minX = 1
    boundaries.minY = 1
    boundaries.maxX = 97
    boundaries.maxY = 97
    val sm = new SectorMatrix(boundaries, SECTOR_PRECISION)
    sm += body
    val res = sm(2, 3).size == 1 && sm(2, 3).exists(_ == body)
    assert(res, s"Body not found in the right sector")
  }

  test("'SectorMatrix.+=' should add a body at (0,97) to the correct bucket of a sector matrix of size 96") {
    val body = new Body(5, 0, 97, 0.1f, 0.1f)
    val boundaries = new Boundaries()
    boundaries.minX = 1
    boundaries.minY = 1
    boundaries.maxX = 97
    boundaries.maxY = 97
    val sm = new SectorMatrix(boundaries, SECTOR_PRECISION)
    sm += body
    val res = sm(0, 7).size == 1 && sm(0, 7).exists(_ == body)
    assert(res, s"Body not found in the right sector")
  }

  test("'SectorMatrix.combine' should combine two matrices") {
    val body1 = new Body(5, 25, 47, 0.1f, 0.1f)
    val body2 = new Body(8, 49, 62, 0.1f, 0.1f)
    val boundaries = new Boundaries()
    boundaries.minX = 1
    boundaries.minY = 1
    boundaries.maxX = 97
    boundaries.maxY = 97
    val sm1 = new SectorMatrix(boundaries, SECTOR_PRECISION)
    val sm2 = new SectorMatrix(boundaries, SECTOR_PRECISION)
    sm1 += body1
    sm2 += body2
    val sm3 = sm1 combine sm2
    val res1 = sm3(2, 3).size == 1 && sm3(2, 3).exists(_ == body1)
    val res2 = sm3(4, 5).size == 1 && sm3(4, 5).exists(_ == body2)
    assert(res1, s"Body 1 not found in the right sector")
    assert(res2, s"Body 2 not found in the right sector")
  }

  object Simulator extends Simulator(null, new TimeStatistics)

  test("'Simulator.updateBoundaries' should update boundaries") {
    val boundaries = new Boundaries()
    boundaries.minX = 12
    boundaries.minY = 12
    boundaries.maxX = 48
    boundaries.maxY = 48
    val body = new Body(5, 8, 64, 0.1f, 0.1f)
    Simulator.updateBoundaries(boundaries, body)
    assert(boundaries.minX == 8, "minX should be 8")
    assert(boundaries.maxX == 48, "minX should be 48")
    assert(boundaries.minY == 12, "minY should be 12")
    assert(boundaries.maxY == 65, "maxY should be 65")
  }

  test("'Simulator.mergeBoundaries' should merge boundaries") {
    val b1 = new Boundaries()
    b1.minX = 12
    b1.minY = 12
    b1.maxX = 48
    b1.maxY = 48
    val b2 = new Boundaries()
    b2.minX = 12
    b2.minY = 8
    b2.maxX = 64
    b2.maxY = 16
    val b3 = new Boundaries()
    b3.minX = 8
    b3.minY = 16
    b3.maxX = 56
    b3.maxY = 2

    val b4 = Simulator.mergeBoundaries(b1, b2)
    val b5 = Simulator.mergeBoundaries(b2, b1)
    assert(b4 == b5, "mergeBoundaries is not commutative")
    assert(b4.minX == 12)
    assert(b4.maxX == 64)
    assert(b4.minY == 8)
    assert(b4.maxY == 48)

    val b6 = Simulator.mergeBoundaries(b1, Simulator.mergeBoundaries(b2, b3))
    val b7 = Simulator.mergeBoundaries(Simulator.mergeBoundaries(b1, b2), b3)
    assert(b6 == b7, "mergeBoundaries is not associative")
  }

  test("'Simulator.computeSectorMatrix' should compute the sector matrix") {
    val body = new Body(5, 0, 97, 0.1f, 0.1f)
    val boundaries = new Boundaries()
    boundaries.minX = 1
    boundaries.minY = 1
    boundaries.maxX = 97
    boundaries.maxY = 97

    val sm = Simulator.computeSectorMatrix(List(body), boundaries)
    val res = sm(0, 7).size == 1 && sm(0, 7).exists(_ == body)
    assert(res, s"Body not found in the right sector")
  }

  test("'Simulator.updateBodies' should update bodies") {
    val b1 = new Body(123f, 18f, 26f, 0f, 0f)
    val b2 = new Body(524.5f, 24.5f, 25.5f, 0f, 0f)
    val b3 = new Body(245f, 22.4f, 41f, 0f, 0f)

    val quad = Leaf(15f, 30f, 20f, Seq(b2, b3))

    val bodies = Simulator.updateBodies(Seq(b1, b2, b3), quad)

    assert(bodies.length == 3)
    assert(bodies.head.xspeed ~= 12.587037f)
    assert(bodies.head.yspeed ~= 0.015557117f)
  }
}

object FloatOps {
  private val precisionThreshold = 1e-4

  /** Floating comparison: assert(float ~= 1.7f). */
  implicit class FloatOps(val self: Float) extends AnyVal {
    def ~=(that: Float): Boolean =
      abs(self - that) < precisionThreshold
  }

  /** Long floating comparison: assert(double ~= 1.7). */
  implicit class DoubleOps(val self: Double) extends AnyVal {
    def ~=(that: Double): Boolean =
      abs(self - that) < precisionThreshold
  }

  /** Floating sequences comparison: assert(floatSeq ~= Seq(0.5f, 1.7f). */
  implicit class FloatSequenceOps(val self: Seq[Float]) extends AnyVal {
    def ~=(that: Seq[Float]): Boolean =
      self.size == that.size &&
        self.zip(that).forall { case (a, b) =>
          abs(a - b) < precisionThreshold
        }
  }

}

