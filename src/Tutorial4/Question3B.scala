package Tutorial4

import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable

class Question3B extends AnyFunSuite {
  def someSet(): mutable.HashSet[String] = mutable.HashSet[String]("AAA", "BBB", "CCC", "DD", "EEE", "FFF")
  def emptySet(): mutable.HashSet[String] = scala.collection.mutable.HashSet[String]()

  test("(Non)empty") {
    assert(someSet().nonEmpty)
    assert(emptySet().isEmpty)
  }
  test("Remove from empty") {
    val s = emptySet()
    assert(!s.remove("A"))
    assert(s.isEmpty)
  }
  test("Contains") {
    val s = someSet()
    assert(s.contains("AAA"))
    assert(!s.contains("ZZZ"))
  }
  test("Nonexistent remove") {
    val s = someSet()
    assert(s.size === 6)
    assert(!s.remove("WWW"))
    assert(s.size === 6)
  }
  test("Duplicate add") {
    val s = someSet()
    assert(s.size === 6)
    assert(!s.add("CCC"))
    assert(s.size === 6)
  }
  test("Actual remove") {
    val s = someSet()
    assert(s.size === 6)
    assert(s.remove("AAA"))
    assert(s.size === 5)
  }
  test("Actual add") {
    val s = someSet()
    assert(s.size === 6)
    assert(s.add("GGG"))
    assert(s.size === 7)
  }
  test("Iterate empty set") {
    val s = emptySet()
    intercept[NoSuchElementException] {
      s.head
    }
  }
  test("Maximum (dictionary)") {
    val s = someSet()
    assert(s.max === "FFF")
  }
  test("Shortest") {
    val s = someSet()
    assert(s.minBy(_.length) === "DD")
  }
}
