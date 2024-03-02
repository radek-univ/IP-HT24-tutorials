package Tutorial5

import scala.reflect.ClassTag

object Question7 {
  trait DoubleEndedQueue[A] {
    /** Is the queue empty?
     * post: q = q₀ ∧ returns q = []
     */
    def isEmpty: Boolean

    /** add x to the start of the queue
     * post : q = x:q₀
     */
    def addLeft(x: A): Unit

    /** get and remove element from the start of the queue.
     * pre: q ≠ []
     * post: returns h such that h:q = q₀
     */
    def getLeft: A

    /** add element to the end of the queue.
     * Post : q = q₀ ++ [x]
     */
    def addRight(x: A): Unit

    /** get and remove element from the end of the queue.
     * pre: q ≠ []
     * post: returns x such that q ++ [x] = q₀
     */
    def getRight: A
  }

  object DoubleLinkedListQueue {
    case class Node[A](_datum: A, var _prev: Node[A] = null, var _next: Node[A] = null)
  }
  // N(from, to):
  //   N(null, _) = []
  //   N(from, to) = if (from == to) [] else from._datum:N(from._next, to)
  // P(from, to):
  //   P(null, _) = []
  //   P(from, to) = if (from == to) [] else from._datum:P(from._prev, to)
  class DoubleLinkedListQueue[A: ClassTag] extends DoubleEndedQueue[A] {
    import DoubleLinkedListQueue._
    // Abs: N(_header.next, _header)
    // DTI: _header ≠ null ∧ N(_header.next, _header) == reverse(P(_header.prev, _header))

    val _header: Node[A] = Node[A](new Array[A](1)(0))
    _header._next = _header
    _header._prev = _header

    override def isEmpty: Boolean = _header._next == _header

    override def addLeft(x: A): Unit = {
      _header._next._prev = Node(x, _header, _header._next)
      _header._next = _header._next._prev
    }

    override def getLeft: A = {
      if (isEmpty)
        throw new IllegalStateException("get on an empty deque")
      val result = _header._next._datum
      _header._next = _header._next._next
      _header._next._prev = _header
      result
    }

    override def addRight(x: A): Unit = {
      _header._prev._next = Node(x, _header._prev, _header)
      _header._prev = _header._prev._next
    }

    override def getRight: A = {
      if (isEmpty)
        throw new IllegalStateException("get on an empty deque")
      val result = _header._prev._datum
      _header._prev = _header._prev._prev
      _header._prev._next = _header
      result
    }
  }

  def main(args: Array[String]): Unit = {
    val deque = new DoubleLinkedListQueue[Int]
    deque.addLeft(5)
    deque.addRight(10)
    println(s"${deque.getLeft}, ${deque.getLeft}")
  }
}
