package Tutorial4

class Question5 {
  /** state: S: 𝒫([0..N))
   * init: S = ∅ */
  trait IntSet {
    val N: Int
    /** Add elem to the set.
     *  pre: 0 <= elem < N
     * post: S = S₀ ∪ {elem} */
    def add(elem: Int): Unit

    /** Does the set contain elem?
     * post: S = S₀ ∧ returns (elem ∈ S) */
    def contains(elem: Int): Boolean

    /** Remove elem from the set.
     * post: S = S₀ ⧵ {elem} */
    def remove(elem: Int): Unit

    /** The size of the set.
     * post: S = S₀ ∧ returns |S| (or #S) */
    def size: Int
  }

  // Abs: { t ∈ [0..N) | _map(t) }       // OurClass  -> state
  //                                     // BitMapSet -> 𝒫([0..N))
  // DTI: 0 <= size < N ∧ _size == |_map.filter(_)| // ∧ |_map| == N
  class BitMapSet(val N: Int) extends IntSet {
    private val _map = new Array[Boolean](N)
    private var _size = 0
    def requireInRange(elem: Int): Unit = require(0 until N contains elem)
    override def add(elem: Int): Unit = {
      requireInRange(elem)
      if (!_map(elem))
        _size += 1
      _map(elem) = true
    }

    override def contains(elem: Int): Boolean = {
      requireInRange(elem)
      _map(elem)
    }

    override def remove(elem: Int): Unit = {
      requireInRange(elem)
      if (_map(elem))
        _size -= 1
      _map(elem) = false
    }

    override def size: Int = {
      // pre ∧ DTI   -- all we know at the beginning
      _size
      // post ∧ DTI  -- what we need to prove at the end
    }
  }

  def main(args: Array[String]): Unit = {

  }
}
