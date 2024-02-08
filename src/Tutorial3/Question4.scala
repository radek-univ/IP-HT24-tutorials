package Tutorial3

object Question4 {

  // Question 4 – insertion sort
  // Predicates:
  // sorted(a) ≡ ∀i,j ∈ [0..|a|). i < j => a(i) <= a(j)                       (is a sorted?)
  //     a ≍ b ≡ |a| == |b| ∧ ∃π ∈ S_|a|. ∀i ∈ [0..|a|). b(π(i+1)-1) == a(i)  (is a permutation of b?)
  //  // Above, S_|a| denotes the symmetric group (https://en.wikipedia.org/wiki/Symmetric_group),
  //  // that is, the set of permutations π: X → X of the set X = {1, 2, 3, ..., |a|}.
  //  Pre: ⊤
  // Post: a ≍ a₀ ∧ sorted(a)
  def insertionSort(a: Array[Int]): Unit = {
    var i = 1
    // i == 1 ∧ sorted(a[0..i)) ∧ a == a₀
    // Inv:  a ≍ a₀ ∧ 1 ≤ i ≤ |a|             ∧ sorted(a[0..i))
    while (i < a.length) {
      //     a ≍ a₀ ∧ 1 ≤ i < |a|             ∧ sorted(a[0..i))
      val p = binarySearch(a, 0, i, a(i))
      //     a ≍ a₀ ∧ 1 ≤ i < |a| ∧ 0 ≤ p ≤ i ∧ sorted(a[0..p) ++ [a(i)] ++ a[p..i))
      cycle(a, p, i + 1)
      //     a ≍ a₀ ∧ 1 ≤ i < |a|             ∧ sorted(a[0..i+1))
      i += 1
      //     a ≍ a₀ ∧ 1 ≤ i ≤ |a|             ∧ sorted(a[0..i)) ≡ Inv
    }
    // ¬(i < a.length) ∧ a ≍ a₀ ∧ 1 ≤ i ≤ |a| ∧ sorted(a[0..i))
    //       a ≍ a₀             ∧ i == |a|    ∧ sorted(a[0..i))
    //       a ≍ a₀                           ∧ sorted(a) ≡ Post
  }

  //  Pre: sorted(a[l..r)) ∧ 0 ≤ l ≤ r ≤ |a|
  // Post: returns j s.t. a[l..j) < v ≤ a[j..r) ∧ l ≤ j ≤ r ∧ a == a₀
  def binarySearch(a: Array[Int], l: Int, r: Int, v: Int): Int = {
    var (i, j) = (l, r)
    while (i < j) {
      val m = i + (j - i) / 2
      if (a(m) < v) {
        i = m + 1
      } else {
        j = m
      }
    }
    i
  }

  //  Pre: 0 <= l < r <= |a|
  // Post: a == a₀[0..l) ++ [a₀(r-1)] ++ a₀[l..r-1) ++ a₀[r..|a|)
  // Example:      AAAAA B
  //   input: [0,1,2,3,4,5,6,7,8]
  //              l↑      r↑
  //          [0,1,5,2,3,4,6,7,8]
  //               B AAAAA
  def cycle(a: Array[Int], l: Int, r: Int): Unit = {
    val v = a(r - 1)
    var i = r - 1
    while (i > l) {
      a(i) = a(i - 1)
      i -= 1
    }
    a(l) = v
  }
}
