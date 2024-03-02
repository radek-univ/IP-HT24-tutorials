package Tutorial5

import scala.reflect.ClassTag

object Question5 {

  /** Original trait
   * state: q: { l ∈ [A] | l is finite }
   *  init: q = [] */
  trait Queue[A] {
    /** Add x to the back of the queue
     * post: q = q₀ ++ [x] */
    def enqueue(x: A): Unit

    /** Remove and return the first element.
     * pre: q ≠ []
     * post: returns h such that q₀ = h:q */
    def dequeue(): A

    /** Is the queue empty?
     * post: q = q₀ ∧ returns q = [] */
    def isEmpty: Boolean
  }

  /**
   * state: q: { l ∈ [A] | |l| ≤ MAX }
   * init: q = [] */
  trait BoundedQueue[A] {
    val MAX: Int

    /** Add x to the back of the queue
     * pre: size < MAX
     * post: q = q₀ ++ [x] */
    def enqueue(x: A): Unit

    /** Remove and return the first element.
     * pre: q ≠ []
     * post: returns h such that q₀ = h:q */
    def dequeue(): A

    /** Is the queue empty?
     * post: q = q₀ ∧ returns q = [] */
    def isEmpty: Boolean

    /** Get the size
     * post: q = q₀ ∧ returns |q| */
    def size: Int
  }

  class ArrayQueue[A: ClassTag](val MAX: Int = 100) extends BoundedQueue[A] {
    val _array = new Array[A](MAX)
    var _start = 0
    var _length = 0
    // Abs: [ _array((_start + i) % MAX) | i <- [0, length) ]
    // DTI: 0 <= _start < MAX ∧ 0 <= _length <= MAX

    @inline def index(i: Int): Int = (_start + i) % MAX

    override def enqueue(x: A): Unit = {
      if (size >= MAX)
        throw new IllegalStateException("enqueue on a full queue")
      _array(index(_length)) = x
      _length += 1
    }

    override def dequeue(): A = {
      if (size <= 0)
        throw new IllegalStateException("dequeue on an empty queue")
      val result = _array(index(0))
      _start = index(1)
      _length -= 1
      result
    }

    override def isEmpty: Boolean = _length == 0

    override def size: Int = _length
  }

  def main(args: Array[String]): Unit = {

  }
}
