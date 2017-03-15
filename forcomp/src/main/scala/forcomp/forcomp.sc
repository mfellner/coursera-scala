import forcomp.Anagrams._

"Foo Zonk@bar".toLowerCase.filter(_.isLetter)

wordOccurrences("Foo Zonk@bar")

dictionary
  .take(1)
  .map(word => wordOccurrences(word) -> List(word))

val abba = List(('a', 2), ('b', 2))

combinations(abba).mkString("\n")

def foo(sentence: Sentence) = {
  (for {
    word <- sentence
    anag <- dictionaryByOccurrences(wordOccurrences(word))
  } yield anag) :::
    (for {
      combos <- combinations(sentenceOccurrences(sentence))
      anag <- dictionaryByOccurrences(combos)
    } yield anag)
}

foo(List("Yes", "man"))

val s: Sentence = List("foo", "bar")
sentenceOccurrences(s)

wordAnagrams("yes")
wordAnagrams("man")
sentenceOccurrences(List("yes", "man")).mkString("\n")
//sentenceAnagrams(List("yes", "man")).mkString("\n")

//val occs = wordOccurrences("yes")
//combinations(occs).mkString("\n")
//
//for {
//  occ <- combinations(wordOccurrences("yes"))
//} yield dictionaryByOccurrences(occ)
//
//dictionaryByOccurrences(List(('e',1), ('s',1), ('y',1)))

//for {
//  combo <- combinations(sentenceOccurrences(List("yes", "man")))
//  word <- dictionaryByOccurrences(combo)
//} yield word


//sentenceOccurrences(List("Roberto", "Carlos"))
