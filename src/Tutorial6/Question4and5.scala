package Tutorial6

object Question4and5 {
  case class Tree(var word: String, var left: Tree = null, var right: Tree = null)

  /** Indent indent steps */
  def indent(indent: Int): String = " . " * indent

  /** Print t, indenting indent steps */
  def toStringRec(t: Tree, depth: Int = 0): String = {
    if (t == null)
      indent(depth) + "null\n"
    else
      s"${indent(depth)}${t.word}\n" +
        toStringRec(t.left, depth + 1) +
        toStringRec(t.right, depth + 1)
  }

  def toStringIter(tree: Tree): String = {
    var result = ""
    val stack = scala.collection.mutable.Stack((tree, 0))
    while (stack.nonEmpty) {
      val (t, depth) = stack.pop()
      if (t == null) {
        result += s"${indent(depth)}null\n"
      } else {
        result += s"${indent(depth)}${t.word}\n"
        stack.push((t.right, depth + 1))
        stack.push((t.left, depth + 1))
      }
    }
    result
  }

  def flip(t: Tree): Unit = {
    if (t != null) {
      val oldLeft = t.left
      t.left = t.right
      t.right = oldLeft
      flip(t.left)
      flip(t.right)
    }
  }

  def main(args: Array[String]): Unit = {
    val tr =
      Tree("three",
        Tree("four",
          Tree("five"),
          Tree("six",
            Tree("seven",
              Tree("one")
            )
          )
        ),
        Tree("two")
      )

    println("Recursive:     ")
    println(toStringRec(tr))
    println("Stack based:   ")
    println(toStringIter(tr))

    flip(tr)
    println("Flipped:     ")
    println(toStringRec(tr))
  }
}
