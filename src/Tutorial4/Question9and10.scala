package Tutorial4

object Question9and10 {
  /**
   * State space: Int → Int
   * Initial state: \x → 0
   */
  trait Bag {
    val MAX: Int

    /** Add x to the bag
     * Pre: 0 <= x < MAX
     * Post: bag = \y → if (x == y) bag₀(y)+1 else bag₀(y)
     * Post: bag = { (key, val) ∈ Int × Int | x ≠ key ∧ val = bag₀(key) ∨ x = key ∧ val = bag₀(key)+1 }
     * Post: bag = bag₀ ⊕ { x → bag₀(x)+1 }
     * All above variants are equivalent, they just use a different notation.
     * */
    def add(x: Int): Unit

    /** How many times does x appear in the bag?
     * Post: bag = bag_0 ∧ returns bag(x) */
    def count(x: Int): Int
  }

  class ArrayBag(val MAX: Int) extends Bag {
    private val c = new Array[Int](MAX)
    // Abs: \x → if (0 ≤ x < MAX) c(x) else 0
    // DTI: ∀i ∈ [0..MAX). c(i) ≥ 0

    def add(x: Int): Unit = {
      require(0 <= x && x < MAX && c(x) < Int.MaxValue)
      c(x) += 1
    }

    def count(x: Int): Int = if (0 <= x && x < MAX) c(x) else 0
  }

  // Bucket sort N numbers
  def sort(a: Array[Int], MAX: Int): Array[Int] = {
    val bag = new ArrayBag(MAX)

    for (el <- a)
      bag.add(el)

    val result = new Array[Int](a.length)
    var j = 0

    for (
      x <- 0 until MAX;
      _ <- 0 until bag.count(x)
    ) {
      result(j) = x
      j += 1
    }
    assert(j == a.length)
    result
  }

}
