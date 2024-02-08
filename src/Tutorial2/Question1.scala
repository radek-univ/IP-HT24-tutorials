package Tutorial2

import org.scalatest.funsuite.AnyFunSuite

object Question1 {
  // Question 1
  // (a) What is the effect of the following procedure?
  // def swap(x: Int, y: Int) = { val t = x; x = y; y = t }

  // (b) What is the effect of the following procedure?
  def swapEntries(a: Array[Int], i: Int, j: Int): Unit = {
    val t = a(i)
    a(i) = a(j)
    a(j) = t
  }
}

class Question1 extends AnyFunSuite {

  // (c) What is the effect of the following procedure?
  def mysteriousFunction(a: Array[Int], i: Int, j: Int): Unit = {
    if (i == j) return
    a(i) ^= a(j)
    a(j) ^= a(i)
    a(i) ^= a(j)
  }

  test("What does mysteriousFunction function do?") {
    val arr = Array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
    val arr2 = arr.clone()
    assert(arr === arr2)
    for ((i, j) <- Array((1, 8), (5, 3), (9, 0), (3, 3), (4, 9), (8, 7))) {
      Question1.swapEntries(arr, i, j)
      mysteriousFunction(arr2, i, j)
      assert(arr === arr2)
    }
  }
}
