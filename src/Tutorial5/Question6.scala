package Tutorial5

import scala.reflect.ClassTag

object Question6 {
  object LinkedListQueue {
    case class Node[A](datum: A, var next: Node[A] = null)
  }
  class LinkedListQueue[A: ClassTag] extends Question5.Queue[A] {
    // Abs: [ n.datum | node <- L(_head.next, null) ]
    // DTI: L(_head) == L(_head, _last) ++ L(_last) ∧ _last.next == null ∧ L(_head) is finite
    import LinkedListQueue._
    var _head: Node[A] = Node[A](new Array[A](1)(0))
    var _last: Node[A] = _head

    /** Add x to the back of the queue.
     * post: q = q₀ ++ [x] */
    override def enqueue(x: A): Unit = {
      _last.next = Node(x, null)
      _last = _last.next
    }

    /** Remove and return the first element.
     * pre: q ≠ []
     * post: returns h such that q₀ = h:q */
    override def dequeue(): A = {
      if (isEmpty)
        throw new IllegalStateException("dequeue on an empty list")
      _head = _head.next
      // _head.next = _head.next.next would be incorrect because of _last!
      _head.datum
    }

    /** Is the queue empty?
     * post: q = q₀ ∧ returns q = [] */
    override def isEmpty: Boolean = _head.next == null
  }

}
