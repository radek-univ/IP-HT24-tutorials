package Tutorial6

import scala.collection.mutable

object Question7b {
  val listOfWords: Array[String] =
    "setal slate stale steal stela tales teals tesla abc cba bca qwerty".split(" ")
  val setOfWords: mutable.Set[String] =
    mutable.HashSet.from(listOfWords)
  val arrayOfSortedPairs: Array[(String, String)] =
    listOfWords.map(x => (x.sorted, x)).sorted

  def binarySearch(a: Array[(String, String)], l: Int, r: Int, v: String): Int = {
    var (i, j) = (l, r)
    while (i < j) {
      val m = i + (j - i) / 2
      if (a(m)._1 < v) {
        i = m + 1
      } else {
        j = m
      }
    }
    i
  }

  def findAnagrams(word: String): List[String] = {
    var p = binarySearch(arrayOfSortedPairs, 0, arrayOfSortedPairs.length, word.sorted)
    var result = List[String]()
    while (p < arrayOfSortedPairs.length && arrayOfSortedPairs(p)._1 == word.sorted) {
      result ::= arrayOfSortedPairs(p)._2
      p += 1
    }
    result
  }

  def main(args: Array[String]): Unit = {
    println(findAnagrams("stale").mkString(", "))
  }
}
