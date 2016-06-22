package org.coursera

package object paraldata {
  def max(xs: Array[Int]): Int =
    xs.par.fold(xs.head)((prev, next) => if (next > prev) next else prev)

  val vowels = Set('a', 'e', 'i', 'o', 'u')

  def countVowels(chars: Array[Char]): Int =
    chars.par.aggregate(0)((n, char) => if (vowels.contains(char.toLower)) n + 1 else n, _ + _)
}
