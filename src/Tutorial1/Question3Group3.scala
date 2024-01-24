package Tutorial1

object Question3Group3 {
  // Pre:  a.length > 0
  // Post: returns max(a[0..a.length))
  def findMax(a: Array[Int]): Int = {
    require(a.length > 0)
    // a.length > 0
    var res = a(0)
    // res == max(a[0,1))
    var i = 1
    // res == max(a[0,1)) ∧ i == 1
    // Inv: res == max(a[0,i)) ∧ 1 <= i <= a.length
    // Var: a.length - i
    while (i < a.length) {
      // res == max(a[0,i)) ∧ 1 <= i <= a.length ∧ i < a.length
      // res == max(a[0,i)) ∧ 1 <= i < a.length
      if (a(i) > res)
        // res == max(a[0,i)) ∧ 1 <= i < a.length ∧ a(i) > res
        res = a(i)
        // res == max(a[0,i+1)) ∧ 1 <= i < a.length
      // (a(i) <= res ∧ res == max(a[0..i)) ∧ 1 <= i < a.length) ∨ (res == max(a[0..i+1)) ∧ 1 <= i < a.length)
      // (res == max(a[0,i+1)) ∧ 1 <= i < a.length) ∨ (res == max(a[0,i+1)) ∧ 1 <= i < a.length)
      //  res == max(a[0,i+1)) ∧ 1 <= i < a.length
      i += 1
      //  res == max(a[0,i)) ∧ 1 <= i <= a.length === Inv
    }
    // res == max(a[0,i)) ∧ 1 <= i <= a.length ∧ i >= a.length
    // res == max(a[0,i)) ∧ i == a.length
    // res == max(a[0..a.length))
    res
    // returns max(a[0..a.length)) === Post
  }
}
