package Tutorial3

object Question2 {
  def sqrt(v: Int): Int = {
    require(v >= 0)
    if (v <= 1)
      return v
    var (l, r) = (0, v)
    // Inv: l² <= v < r² ∧ 0 <= l < r
    while (l + 1 < r) {
      val m1 = l + (r - l) / 3
      val m2 = l + 2 * (r - l) / 3

      if (m2 * m2 <= v) {
        l = m2
      } else if (m1 * m1 <= v) {
        l = m1
        r = m2
      } else {
        r = m1
      }
    }
    // Inv ∧ ¬(l+1 < r)
    // l² <= v < r² ∧ 0 <= l ∧ r = l + 1
    // l² <= v < (l+1)² ∧ 0 <= l
    l
  }

  // - Why are the `if` conditions correct?
  // - Can this still overflow?
  def sqrtLessOverflow(y: Int): Int = {
    require(y >= 0)
    if (y <= 1)
      return y
    var (a, b) = (0, y)
    while (a + 1 < b) {
      val m1 = a + (b - a) / 3
      val m2 = if ((b - a) >= Int.MaxValue / 2) { // If the interval is wide...
        a + ((b - a) / 3) * 2 // Approximate
      } else {
        a + 2 * (b - a) / 3 // Precise
      }
      // val m2 = b - (b-a) / 3

      if (m2 <= y / m2) {
        // Condition m2 * m2 <= v was replaced with m2 <= y / m2 is this correct?
        // Fix any y ∈ ℕ. Let A,B ∈ ℕ be the unique numbers such that
        //   y == A² + B   and   B ∈ [0..2A]
        // What is the threshold for m2 when these conditions stop being true?
        //                       ┌─────────┬───────────┐
        //                       │ m2 == A │ m2 == A+1 │
        // ┌─────────────────────┼─────────┼───────────┤
        // │ m2 * m2 <= A² + B   │ true    │ false     │
        // ├─────────────────────┼─────────┼───────────┤
        // │ m2 <= (A² + B) / m2 │ true    │ false¹    │
        // └─────────────────────┴─────────┴───────────┘
        // ¹/ (A² + B) / (A+1) <= (A² + 2A) / (A+1) == ((A+1)² - 1) / (A+1) < (A+1)² / (A+1) == m2
        a = m2
      } else if (m1 <= y / m1) {
        a = m1
        b = m2
      } else {
        b = m1
      }
    }
    a
  }
}
