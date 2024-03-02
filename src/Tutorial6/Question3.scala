package Tutorial6

object Question3 {
  object ClosedHash {
    private case class Entry(word: String, var count: Int)
  }

  class ClosedHash(val MAX: Int) {

    import ClosedHash.Entry

    private val table = new Array[Entry](MAX)

    // DTI: if word appears at position i, there will be no null in
    // positions [hash(word)..i) (wrapping round).  Also no word appears twice
    // (even with a count of 0).

    private def hash(word: String): Int = {
      def f(e: Int, c: Char) = (e * 41 + c.toInt) % MAX

      word.foldLeft(1)(f)
    }

    /** Find the position where word is stored, or (if word doesn't appear in
     * the table) an index pointing at null where word should be stored.
     * Returns None if the table is full and the word was not found. */
    sealed trait FindResult
    case class Index(i: Int) extends FindResult
    case class InsertionPoint(i: Int) extends FindResult
    case object ArrayFull extends FindResult

    private def find(word: String): FindResult = {
      val h = hash(word)
      var i = h
      // Invariant: ???
      while (table(i) != null && table(i).word != word) {
        i = (i + 1) % MAX
        if (i == h)
          return ArrayFull
      }
      if (table(i) == null)
        InsertionPoint(i)
      else
        Index(i)
    }

    /** Add word to the bag */
    def add(word: String): Unit = find(word) match {
      case ArrayFull         => throw new IllegalStateException("Hash table full")
      case Index(i)          => table(i).count += 1
      case InsertionPoint(i) => table(i) = Entry(word, 1)
    }

    /** How many times does word appear? */
    def count(word: String): Int = find(word) match {
      case Index(i) => table(i).count
      case _        => 0
    }

    /** Delete one occurrence of word */
    def trivialDelete(word: String): Unit = find(word) match {
      case Index(i) if table(i).count > 0 => table(i).count -= 1
      case _                              => ()
    }

    /** Delete one occurrence of word and remove tuple if last occurrence removed */
    def delete(word: String): Unit = find(word) match {
      case Index(i) if table(i).count > 1 => table(i).count -= 1
      case Index(i)                       => removeAndFillTheGap(i)
      case _                              => ()
    }

    // var Index(x) = Index(10)

    def removeAndFillTheGap(i: Int): Unit = {
      var gap = i
      var j = (gap + 1) % MAX
      // I: (table(k) != null && hash(table(k).word) in [gap+1..j))
      //    for all k in [gap+1, j)  (treated as a circular arrays)

      // post: returns x ∈ (a..b] (treated as a circular array range)
      def betweenModulo(x: Int, a: Int, b: Int) = {
        if (a <= b)       // [0....a....b....]MAX
          a < x && x <= b //        ^^^^^
        else              // [0....b....a....]MAX
          a < x || x <= b //  ^^^^^^     ^^^^
      }

      // We cannot fill in the gap if h ∈ (gap..j] cyclically, i.e.:
      //           gap < h <= j
      //       j < gap < h
      //  h <= j < gap
      while (table(j) != null && j != i) {
        val h = hash(table(j).word)
        if (!betweenModulo(h, gap, j)) { // table(j) not in the right block
          table(gap) = table(j)
          gap = j
        }
        j = (j + 1) % MAX
      }
      table(gap) = null
    }

    override def toString: String = {
      var st = ""
      for (i <- 0 until MAX)
        if (table(i) != null)
          st += s"(${table(i).word}, ${table(i).count})"
      st
    }

    def hashTableToString: String = {
      "\nhash table: " + table.map({ case Entry(word, count) => (word, hash(word), count); case null => null }).mkString(", ")
    }
  }

  def main(args: Array[String]): Unit = {
    val h = new ClosedHash(6)
    h.add("A") // 4
    // h.add("B") // 5
    // h.add("C") // 0
    // h.add("D") // 1
    // h.add("E") // 2
    // h.add("F") // 3
    h.add("H") // 5
    h.add("G") // 4
    h.add("I") // 0
    println(h.hashTableToString)
    h.delete("A")
    println(h.hashTableToString)
  }
}
