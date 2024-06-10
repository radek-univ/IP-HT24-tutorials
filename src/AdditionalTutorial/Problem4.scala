package AdditionalTutorial

import scala.annotation.tailrec

object Problem4 {
  // Helper class representing an open interval
  case class Interval(from: Float, to: Float) {
    require(from <= to)

    // returns true whenever "that" contains only numbers grater that "this"
    def <<<(that: Interval): Boolean = to <= that.from // remember that the intervals are open, so we need ≤

    // returns true whenever "that" contains only numbers smaller that "this"
    def >>>(that: Interval): Boolean = that.to <= from

    def intersects(that: Interval): Boolean = !(this <<< that || that <<< this)

    override def toString: String = f"($from, $to)"
  }

  // abstract state:
  //     S: 𝒫( Intervals )
  //     where Intervals is the set of all open intervals with Float endpoints:
  //       Intervals = { (a, b) ⊆ Float | a,b ∈ Float ∧ a ≤ b }
  // initial:
  //     S₀ = ∅
  // abstract DTI:
  //   ∙ All intervals are pairwise-disjoint:
  //     ∀I,J ∈ S. I ≠ J  ==>  I ∩ J = ∅
  trait DisjointIntervalSet {
    // precondition:  none
    // postcondition:
    //     ⋃S = ⋃S₀ ∪ I
    //     Sum of elements of S after adding
    //        is equal to I plus the sum of elements of S₀
    def addInterval(I: Interval): Unit

    // precondition:  none
    // returns Res such that:
    //     (x ∈ Res ∧ Res ∈ S  ∨  Res == null ∧ x ∉ ⋃S) ∧ S = S₀
    def getInterval(x: Float): Interval
  }

  // Abstraction function:
  //     It maps the concrete implementation to the abstract state:
  //     absFun: TreeDisjointIntervalSet --> 𝒫( Intervals )
  //     and is defined as:
  //     absFun(obj) = { (n.interval.from, n.interval.to) | n ∈ T(obj.root) }
  //         where T(n) is the tree equivalent of the L function for linked lists,
  //         which collects the nodes of the tree in an "in-order" fashion:
  //             T: Node --> [Node]
  //             T(null) = []
  //             T(n) = T(n.left) ++ [n] ++ T(n.right)
  // Datatype invariant:
  //   ∙ Intervals in the tree are sorted:
  //     [ a | n <- T(root), a <- [n.interval.from, n.interval.to]] is sorted
  //          we need this to reliably navigate the tree
  class TreeDisjointIntervalSet extends DisjointIntervalSet {
    // Enumeration type with two values: Left and Right.
    // It represents the direction of traversing the tree.
    // It is isomorphic to the Boolean type, but the names are meaningful in
    // our context, which improves readability: Left --- True, Right --- False.
    object Direction extends Enumeration {
      type Direction = Value
      val Left, Right = Value
    }

    import Direction._

    private def opposite(dir: Direction): Direction = if (dir == Left) Right else Left

    case class Node(var left: Node, var interval: Interval, var right: Node) {
      // The following two methods enable direction-parameterised
      // get/set operations for the children of a node:
      // node(Left) = x
      // y = node(Right)
      def apply(dir: Direction): Node = {
        if (dir == Left) left else right
      }

      def update(dir: Direction, node: Node): Unit = {
        if (dir == Left) left = node else right = node
      }
    }

    var root: Node = null

    override def addInterval(I: Interval): Unit = {
      if (root == null) { // Special case when the tree is empty.
        root = Node(null, I, null)
      } else {
        addIntervalHelper(I, root, null, Left)
      }
    }

    @tailrec
    private def addIntervalHelper(I: Interval, node: Node, prevNode: Node, dir: Direction): Unit = {
      if (node == null) { // node is null, add a new node as a child of prevNode
        prevNode(dir) = Node(null, I, null)
      } else if (I <<< node.interval) { // interval I is to the left of current node's interval
        addIntervalHelper(I, node.left, node, Left)
      } else if (I >>> node.interval) { // symmetrically, I is to the right
        addIntervalHelper(I, node.right, node, Right)
      } else { // I is not to the left or right, so it intersects with the current interval
        // Delete intervals intersecting with I in the subtrees of node, remembering the extremal values
        val (leftTree, leftMinVal) = deleteIntersecting(I, node.left, Left)
        val (rightTree, rightMaxVal) = deleteIntersecting(I, node.right, Right)
        // Use the extremal values to compute the interval to be inserted
        val left = Math.min(leftMinVal, Math.min(I.from, node.interval.from))
        val right = Math.max(rightMaxVal, Math.max(I.to, node.interval.to))
        // Update the current node
        node.interval = Interval(left, right)
        node.left = leftTree
        node.right = rightTree
      }
    }

    private def deleteIntersecting(I: Interval, currNode: Node, dir: Direction): (Node, Float) = {
      if (currNode == null) {
        // We have nothing to delete, so we return null, and as the extremal value
        // we return something which will not change the result of min/max up in the tree
        val extremalValue = if (dir == Left) Float.PositiveInfinity else Float.NegativeInfinity
        (null, extremalValue)
      } else if (currNode.interval.intersects(I)) {
        // Current interval intersects with I, and we came from the direction dir.
        // This means we can discard the subtree currNode(opposite(dir)) and the interval currNode.interval.
        // Therefore, only compute the updatedSubtree based on currNode(dir).
        val (updatedSubtree, value) = deleteIntersecting(I, currNode(dir), dir)
        val value2 = if (dir == Left) Math.min(currNode.interval.from, value) else Math.max(currNode.interval.to, value)
        (updatedSubtree, value2)
      } else {
        // Current interval does not intersect with I, so it needs to stay in the tree.
        // Descend in the direction opposite(dir) and delete intervals there, the whole subtree currNode(dir) must be preserved.
        val (updatedSubtree, value) = deleteIntersecting(I, currNode(opposite(dir)), dir)
        currNode(opposite(dir)) = updatedSubtree
        (currNode, value)
      }
    }

    override def getInterval(x: Float): Interval = getIntervalHelper(Interval(x, x), root)

    @tailrec
    private def getIntervalHelper(I: Interval, currNode: Node): Interval = {
      if (currNode == null) null
      else if (I <<< currNode.interval) getIntervalHelper(I, currNode.left)  // go to the left
      else if (I >>> currNode.interval) getIntervalHelper(I, currNode.right) // go to the right
      else currNode.interval // return this interval
    }

    // Helper functions for pretty-printing
    def node2string(n: Node): String = {
      if (n == null) ""
      else f"${node2string(n.left)}${n.interval.toString} <<< ${node2string(n.right)}"
    }

    override def toString: String = node2string(root).dropRight(4)
  }

  // Some code to test the interval tree.
  def main(args: Array[String]): Unit = {
    val t = new TreeDisjointIntervalSet
    t.addInterval(Interval(11, 20))
    println(t)
    t.addInterval(Interval(15, 30))
    println(t)
    t.getInterval(21)
    val list = List(Interval(1.0f, 2.0f), Interval(3, 4), Interval(5, 6), Interval(7, 8), Interval(9, 10), Interval(3.6f, 9.2f))
    for (i <- list) {
      t.addInterval(i)
      println(t)
    }
  }
}
