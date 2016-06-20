package coursera.progfun

import scala.io.Source

package object phone {
  lazy val in = Source.fromURL("http://lamp.epfl.ch/files/content/sites/lamp/files/teaching/progfun/linuxwords.txt")
  lazy val words = in.getLines filter (_.toUpperCase.forall(charCode.contains))

  val mnem = Map(
    '2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
    '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")

  val charCode: Map[Char, Char] = for {
    (n, chars) <- mnem
    char <- chars
  } yield (char, n)

  def wordCode(word: String): String = word.toUpperCase map charCode

  val wordsForNum: Map[String, Seq[String]] =
    words.toList groupBy wordCode withDefaultValue Seq()

  def encode(number: String): Set[List[String]] = number match {
    case "" => Set(List())
    case s => (for {
      split <- 1 to number.length
      word <- wordsForNum(number take split)
      rest <- encode(number drop split)
    } yield word :: rest).toSet
  }

  def translate(number: String): Set[String] = encode(number).map(_ mkString " ")
}
