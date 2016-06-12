
import common._

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))

    def apply(x: Int, y: Int): RGBA = data(y * width + x)

    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    val x0 = x - radius
    var xi = x0
    var yi = y - radius
    val xn = x + radius
    val yn = y + radius
    var n = 0
    var r: Int = 0
    var g: Int = 0
    var b: Int = 0
    var a: Int = 0

    while (yi <= yn) {
      while (xi <= xn) {
        if (xi < src.width && xi >= 0 && yi < src.height && yi >= 0) {
          val c = src(xi, yi)
          n += 1
          r += red(c)
          g += green(c)
          b += blue(c)
          a += alpha(c)
        }
        xi += 1
      }
      xi = x0
      yi += 1
    }
    rgba(r / n, g / n, b / n, a / n)
  }

  def split(xs: Seq[Int], tasks: Int): Seq[(Int, Int)] = {
    if (tasks < 2) List((xs.head, xs.last))
    else {
      val n = xs.length / 2
      val t = tasks.toDouble / 2
      val t0 = math.ceil(t).toInt
      val t1 = math.floor(t).toInt
      split(xs take n + 1, t0) ++ split(xs drop n, t1)
    }
  }

  def slices(size: Int, tasks: Int): Seq[(Int, Int)] = split(0 to size, tasks)

}
