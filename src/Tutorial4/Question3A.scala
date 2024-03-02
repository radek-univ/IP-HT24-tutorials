package Tutorial4

import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable

class Question3A extends AnyFunSuite {
  // Question: Will both tests pass?
  // Not if we run them out of order.
  private val s = mutable.HashSet[String]("AAA", "BBB")
  test("adding") {
    assert(s.size === 2)
    assert(s.add("CCC"))
    assert(s.size === 3)
  }
  test("removing") {
    assert(s.size === 3)
    assert(s.remove("AAA"))
    assert(s.size === 2)
  }
}
