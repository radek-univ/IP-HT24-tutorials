package Tutorial3

object Question7 {
  def QSort(a: Array[Int], L: Int, R: Int): Unit = {
    // - How to minimise the call stack usage?
    var (l, r) = (L, R)
    while (r - l > 1) {
      val k = Question6.partition(a, l, r)
      // QSort(a, l, k); QSort(a, k + 1, r)
      if (k - l < r - k - 1) {
        // Left interval shorter
        QSort(a, l, k)
        l = k + 1 // ≂ QSort(a, k + 1, r)
      } else {
        // Right interval shorter
        QSort(a, k + 1, r)
        r = k     // ≂ QSort(a, l, k)
      }
      l = k + 1
    }
  }
}
