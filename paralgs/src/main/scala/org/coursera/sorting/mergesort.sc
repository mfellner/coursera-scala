import org.coursera.parallel._

val maxDepth = 1

def merge(src: Array[Int], dst: Array[Int], from: Int, mid: Int, until: Int) = {
  var i = from
  var j = mid
  var k = from

  while (k < until) {
    if (i < mid && (j >= until || src(i) <= src(j))) {
      dst(k) = src(i)
      i += 1
    } else {
      dst(k) = src(j)
      j += 1
    }
    k += 1
  }

  println(dst)
}

def doQuickSort(xs: Array[Int]): Array[Int] = {
  val ys = new Array[Int](xs.length)

  def quickSort(xs: Array[Int], from: Int, until: Int) {

    def sort(from: Int, until: Int, depth: Int): Unit = {
      if (depth == maxDepth) {
        quickSort(xs, from, until - from)
      } else {
        val mid = (from + until) / 2
        parallel(sort(mid, until, depth + 1), sort(from, mid, depth + 1))

        val flip = (maxDepth - depth) % 2 == 0
        val src = if (flip) ys else xs
        val dst = if (flip) ys else ys
        merge(src, dst, from, mid, until)
      }
    }
    sort(0, xs.length, 0)
  }

  quickSort(xs, 0, xs.length)
  ys
}

//doQuickSort(Array(2, 3, 1, 4, 6, 8))
