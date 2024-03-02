package Tutorial5

import Tutorial5.Question5.{ArrayQueue, BoundedQueue, Queue}
import org.scalatest.funsuite.AnyFunSuite


class Question5Tests extends AnyFunSuite {
  test("Question 5 – queue") {
    val queue: BoundedQueue[String] = new ArrayQueue[String](4)
    queue.enqueue("A")
    assert(queue.size === 1)
    queue.enqueue("B")
    assert(queue.size === 2)
    queue.enqueue("C")
    assert(queue.size === 3)
    queue.enqueue("D")
    assert(queue.size === 4)
    intercept[IllegalStateException](queue.enqueue("E"))
    assert(queue.dequeue() === "A")
    assert(queue.size === 3)
    assert(queue.dequeue() === "B")
    assert(queue.size === 2)
    queue.enqueue("E")
    assert(queue.size === 3)
    queue.enqueue("F")
    assert(queue.size === 4)
    intercept[IllegalStateException](queue.enqueue("G"))
    assert(queue.dequeue() === "C")
    assert(queue.size === 3)
    assert(queue.dequeue() === "D")
    assert(queue.size === 2)
    assert(queue.dequeue() === "E")
    assert(queue.size === 1)
    assert(queue.dequeue() === "F")
    assert(queue.size === 0)
    assert(queue.isEmpty)
    intercept[IllegalStateException](queue.dequeue())
  }
}
