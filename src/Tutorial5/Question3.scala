package Tutorial5

import org.scalatest.run

object Question3 {
  /**
   * State: book: String ⇸ String
   * Init:  book = ∅ */
  trait Book {
    /** Add the maplet name -> number to the mapping.
     * Post: book = book₀ ⊕ {name -> number} */
    def store(name: String, number: String): Unit

    /** Return the number stored against name.
     * Pre: name ∈ dom(book)
     * Post book = book₀ ∧ returns book(name) */
    def recall(name: String): String

    /** Does name appear in the book?
     * Post: book = book₀ ∧ returns name ∈ dom(book) */
    def isInBook(name: String): Boolean

    /** Delete the number stored against name (if it exists).
     * post: returns (name ∈ dom(book₀)) ∧
     * book = book₀ ⧵ { name -> book₀(name) } */
    def delete(name: String): Boolean

    /**
     * post: returns |dom(book)| ∧ book = book₀ */
    def size: Int
  }

  object LinkedListHeaderOrderedBook {
    private class Node(var name: String, var number: String, var next: Node)
  }

  class LinkedListHeaderOrderedBook extends Book {

    import LinkedListHeaderOrderedBook.Node

    private val _list = new Node("", "", null)
    private var _size = 0

    private def find(name: String): Node = {
      var n = _list
      while (n.next != null && n.next.name < name)
        n = n.next
      n
    }

    @inline
    private def found(name: String, result: Node): Boolean =
      result.next != null && result.next.name == name

    override def store(name: String, number: String): Unit = {
      val n = find(name)
      if (found(name, n)) {
        n.next.number = number
      } else {
        n.next = new Node(name, number, n.next)
        _size += 1
      }
    }

    override def recall(name: String): String = {
      val n = find(name)
      if (!found(name, n))
        throw new IllegalStateException("recall on a key which was not in the book")
      n.next.number
    }

    override def isInBook(name: String): Boolean = {
      val n = find(name)
      found(name, n)
    }

    override def delete(name: String): Boolean = {
      val n = find(name)
      if (!found(name, n))
        return false
      n.next = n.next.next
      _size -= 1
      true
    }

    override def size: Int = _size
  }

  def main(args: Array[String]): Unit = {

  }
}
