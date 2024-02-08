package Tutorial3

import Tutorial2.Question1.{swapEntries => swap}

object Question6 {
  def partition(a: Array[Int], l: Int, r: Int): Int = {
    val pivot = a(l)
    var (i, j) = (l + 1, r)
    while (i < j) {
      if (a(i) < pivot) {
        i += 1
      } else if (a(j - 1) >= pivot) {
        j -= 1
      } else {
        swap(a, i, j - 1)
        i += 1
        j -= 1
      }
    }
    // move pivot to the correct position
    a(l) = a(i - 1)
    a(i - 1) = pivot
    i - 1
  }

  def main(args: Array[String]): Unit = {
    val a = Array(4, 1, 6, 8, 3, 4, 2, 3, 9, 0)
    partition(a, 0, a.length)
    println(a.mkString("Array(", ", ", ")"))
  }
}
