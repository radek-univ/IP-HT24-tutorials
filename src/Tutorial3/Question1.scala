package Tutorial3

object Question1 {
  // b) N = 0; Is postcondition satisfied?
  // a) Can you think of a weaker postcondition that would characterise the result for general (not necessarily sorted) arrays?
  // Pre: ———————————— (removed)
  // Post: returns i s.t. a(i-1) < x <= a(i)
  // (assuming that a(-1) == -∞ and a(|a|) == ∞)
  def search(a: Array[Int], x: Int): Int = {
    var (i, j) = (0, a.length)
    while (i < j) {
      val m = (i + j) / 2
      if (a(m) < x)
        i = m + 1
      else
        j = m
    }
    i
  }

  def main(args: Array[String]): Unit = {
    // search(Array(1,2,3,5,7,8,9,11,14,15,15,23,42,56), 25)
    search(Array(15, 23, 11, 14, 42, 56, 1, 2, 3, 5, 7, 8, 9, 15), 25)
  }
}
