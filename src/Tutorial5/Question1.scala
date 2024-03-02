package Tutorial5

import scala.annotation.tailrec

object Question1 {
  case class Node(datum: Int, var next: Node) {
    override def toString: String = toStringHelper(this)
  }

  @tailrec def toStringHelper(l: Node, prefix: String = ""): String = l match {
    case Node(h, null) => s"$prefix$h."
    case Node(h, t)    => toStringHelper(t, s"$prefix$h -> ")
    case null          => "Empty"
  }

  def constructList(N: Int = 12): Node = {
    var list: Node = null
    for (i <- 1 to N)
      list = Node(i, list)
    list
  }

  //  Pre: L(node) is finite
  // Post: returns x s.t. L(x) = reversed(L(node₀))
  def reverseList(node: Node): Node = {
    var left: Node = null
    var right = node
    // Inv: L(node₀) = reversed(L(left)) ++ L(right) ∧ L(right) is finite
    // Var: |right|
    // Example: After a few steps, we will have
    // 1, 2, 3, 4, 5, 6, 7, 8
    // 4 -> 3 -> 2 -> 1 = left
    // 5 -> 6 -> 7 -> 8 = right
    while (right != null) {
      val current = right
      right = current.next
      current.next = left
      left = current
    }
    // L(node₀) = reversed(L(left)) ++ L(right) ∧ right == null
    // L(node₀) = reversed(L(left)) ++ []
    // L(node₀) = reversed(L(left))
    left
  }

  def main(args: Array[String]): Unit = {
    val l1 = constructList()
    println(s"List is $l1")
    val l2 = reverseList(l1)
    println(s"Reversed list is $l2")
  }
}