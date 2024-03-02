package Tutorial6

import scala.collection.mutable

object Question7a {
  val listOfWords: Array[String] = "setal slate stale steal stela tales teals tesla abc cba bca qwerty".split(" ")
  val setOfWords: mutable.Set[String] = mutable.HashSet.from(listOfWords)

  // Question: How to avoid generating duplicates?
  def perms(s: String): List[String] = {
    if (s == "")
      return List(s)
    var result = List[String]()
    for (p <- perms(s.tail))
      for ((pref, tail) <- p.inits.toList.reverse zip p.tails)
        if (!pref.contains(s.head)) // Answer: by adding this 'if'
          result ::= pref + s.head + tail

    result
  }

  def findAnagramsA(word: String): List[String] = {
    perms(word).filter(x => setOfWords.contains(x))
  }

  def main(args: Array[String]): Unit = {
    println(perms("aab").mkString(", "))
  }
}
