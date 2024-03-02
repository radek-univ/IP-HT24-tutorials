package Tutorial5

import Tutorial5.Question3.Book
import org.scalatest.funsuite.AnyFunSuite

class Question3Tests(newBook: () => Book) extends AnyFunSuite {
  def setup(): Book = {
    val book = newBook()
    book.store("Gavin", "1234")
    book.store("Pete", "5678")
    book
  }

  test(s"recall of first stored value") {
    val book = setup()
    assert(book.isInBook("Gavin"))
    assert(book.recall("Gavin") === "1234")
  }

  test("recall of second stored value") {
    val book = setup()
    assert(book.isInBook("Pete"))
    assert(book.recall("Pete") === "5678")
  }

  test("recall of unstored value") {
    val book = setup()
    assert(!book.isInBook("Mike"))
    intercept[IllegalArgumentException] {
      book.recall("Mike")
    }
  }

  test("recall of unstored value that could be a dummy header") {
    val book = setup()
    assert(!book.isInBook(""))
    assert(!book.isInBook("?"))
  }

  test("delete of unstored value") {
    val book = setup()
    assert(book.delete("Joe") === false)
  }

  test("delete of stored value") {
    val book = setup()
    assert(book.isInBook("Gavin"))
    assert(book.delete("Gavin"))
    assert(!book.isInBook("Gavin"))
  }

  test("delete of only stored value") {
    val book = setup()
    assert(book.isInBook("Pete"))
    assert(book.delete("Pete"))
    assert(!book.isInBook("Pete"))
  }
}
