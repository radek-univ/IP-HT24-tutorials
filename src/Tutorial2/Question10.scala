package Tutorial2

class Question10 {
  // Why is limiting the number of multiplications helpful?
  //  Pre: ⊤
  // Post: returns ∑_{0 <= i < |a|} a(i)⋅xⁱ
  def eval(a: Array[Double], x: Double): Double = {
    var i = 0
    var res = 0.0
    // Inv:
    while (i < a.length) {
      res *= x
      res += a(i)
      i += 1
    }
    res
  }
}
