import kmeans.Point
import kmeans.KMeans

import scala.collection.{GenMap, GenSeq}

object KM extends KMeans

import KM._

val p1 = new Point(1, 1, 0)
val p2 = new Point(1, -1, 0)
val p3 = new Point(-1, 1, 0)
val p4 = new Point(-1, -1, 0)
val points: GenSeq[Point] = IndexedSeq(p1, p2, p3, p4)
val mean1 = new Point(1, 0, 0)
val mean2 = new Point(-1, 0, 0)
val means: GenSeq[Point] = IndexedSeq(mean1, mean2)
val mean3 = new Point(0.5, 0, 0)
val mean4 = new Point(-0.5, 0, 0)
val oldMeans: GenSeq[Point] = IndexedSeq(mean3, mean4)
val classified = GenMap((mean1, GenSeq(p1, p2)), (mean2, GenSeq(p3, p4)))


converged(0.5)(oldMeans, means)
