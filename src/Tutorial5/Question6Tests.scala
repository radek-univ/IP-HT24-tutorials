package Tutorial5

import Tutorial5.Question5.Queue
import Tutorial5.Question6.LinkedListQueue
import org.scalatest.funsuite.AnyFunSuite

class Question6Tests extends AnyFunSuite {
  test("Question 6 – linked list queue") {
    val queue: Queue[String] = new LinkedListQueue[String]()
    queue.enqueue("A")
    assert(!queue.isEmpty)
    queue.enqueue("B")
    assert(!queue.isEmpty)
    queue.enqueue("C")
    assert(!queue.isEmpty)
    queue.enqueue("D")
    assert(!queue.isEmpty)
    assert(queue.dequeue() === "A")
    assert(!queue.isEmpty)
    assert(queue.dequeue() === "B")
    assert(!queue.isEmpty)
    queue.enqueue("E")
    assert(!queue.isEmpty)
    queue.enqueue("F")
    assert(!queue.isEmpty)
    assert(queue.dequeue() === "C")
    assert(!queue.isEmpty)
    assert(queue.dequeue() === "D")
    assert(!queue.isEmpty)
    assert(queue.dequeue() === "E")
    assert(!queue.isEmpty)
    assert(queue.dequeue() === "F")
    assert(queue.isEmpty)
    intercept[IllegalStateException](queue.dequeue())
  }
}
