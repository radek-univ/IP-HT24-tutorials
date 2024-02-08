package Tutorial2

object Question7 {
  //  Pre: ⊤
  // Post: ∃j∈[0..N). p(j)
  def exists(p: Int => Boolean, N: Int): Boolean = {
    require(N >= 0)
    var i = 0
    // Inv: 0<=i<=N ∧ ∀j∈[0..i). ¬p(j)
    while (i < N && !p(i)) i += 1
    i < N
  }
}
