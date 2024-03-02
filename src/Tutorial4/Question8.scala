package Tutorial4

object Question8 {
  /**
   * State: book: { f: String ⇸ String | |dom(f)| ≤ MAX }
   * partial functions taking a String and outputting a String, whose domain size is at most MAX
   * Init:  book = ∅ */
  trait Book {
    val MAX: Int
    /** Add the maplet name -> number to the mapping.
     * Pre: size < MAX
     * Post: book = book₀ ⊕ {name -> number} */
    def store(name: String, number: String): Unit

    /** Return the number stored against name.
     * Pre: name ∈ dom(book)
     * Post book = book₀ ∧ returns book(name) */
    def recall(name: String): String

    /** Does name appear in the book?
     * Post: book = book₀ ∧ returns name ∈ dom(book) */
    def isInBook(name: String): Boolean

    /** Added for tutorial question */

    /** Delete the number stored against name (if it exists).
     * post: returns (name ∈ dom(book₀)) ∧
     * book = book₀ ⧵ { name -> book₀(name) } */
    def delete(name: String): Boolean

    /**
     * post: returns |dom(book)| ∧ book = book₀ */
    def size: Int
  }

  class OrderedArraysBook(val MAX: Int) extends Book {
    private val names = new Array[String](MAX)
    private val numbers = new Array[String](MAX)
    private var count = 0

    override def size: Int = count
    // ABS: { names(i) -> numbers(i) | i ∈ [0..count) }
    // DTI: 0 <= count < MAX ∧ ∀ i,j ∈ [0..count). i < j => names(i) < names(j)

    /** Post: book = book₀ ∧ returns the index i ≤ count s.t. names[0..i) < name ≤ names[i..count). */
    private def find(name: String): Int = {
      // Inv: book = book₀ ∧ names[0..i) < name ≤ names[j..count) ∧ 0 <= i <= j <= count
      var i = 0
      var j = count
      while (i < j) {
        val m = (i + j) / 2
        if (names(m) < name) i = m + 1 else j = m
      }
      i
    }

    def recall(name: String): String = {
      val i = find(name)
      require(i < count && names(i) == name, s"Name $name not found")
      numbers(i)
    }

    def store(name: String, number: String): Unit = {
      val i = find(name)
      if (i < count && names(i) == name) {
        numbers(i) = number
        return
      }

      if (count >= MAX)
        throw new IllegalStateException("Maximum size exceeded")

      for (j <- count - 1 to i by -1) {
        names(j + 1) = names(j)
        numbers(j + 1) = numbers(j)
      }
      names(i) = name
      numbers(i) = number
      count += 1
    }

    def isInBook(name: String): Boolean = {
      val i = find(name)
      i < count && names(i) == name
    }

    def delete(name: String): Boolean = {
      val p = find(name)
      if (p >= count || names(p) != name)
        return false

      for (j <- p until count - 1) {
        names(j) = names(j + 1)
        numbers(j) = numbers(j + 1)
      }
      count -= 1
      true
    }
  }
}
