package Tutorial2

import org.scalatest.funsuite.AnyFunSuite

class Question3 extends AnyFunSuite {
  test("test1") {
    val a = Array("Aaa", "bbb", "ccc", "ddd")
    val b = Array("ccc", "ddd", "Aaa", "bbb")
    assert(b.sorted === a)
  }

  val arrays: Array[Array[String]] = Array(
    "This is just some sentence in English.".split(" "),
    "Monday Tuesday Wednesday Thursday Friday Saturday Sunday".split(" "),
    Array("1", "1 0", "1 3", "1 2", "1  2")
  )

  def quick(a: Array[String]): Array[String] = {
    if (a.length < 2)
      return a
    val pivot = a(0)
    quick(a.filter(_ < pivot)) appended pivot appendedAll quick(a.filter(_ > pivot))
  }

  test("Is sorting correct") {
    for (array <- arrays)
      assert(array.sorted === quick(array))
  }

  test("Is reverse lexicographic sorting correct") {
    for (array <- arrays)
      assert(array.sortWith(_ > _) === quick(array).reverse)
  }

  test("Is sorting by length correct") {
    for (array <- arrays) {
      val result = array.sortBy(_.length)
      val lengths = result.map(_.length)
      assert(lengths.zip(lengths.tail).forall { case (x, y) => x <= y })
    }
  }

  test("Is sorting and filtering commutative") {
    val f = (x: String) => x.nonEmpty && x(0).isUpper

    for (array <- arrays) {
      val expected = quick(array)
      assert(array.filter(f).sorted === expected.filter(f))
    }
  }
}
