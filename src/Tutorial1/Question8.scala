package Tutorial1

object Question8 {
  //  Pre: none
  // Post: returns a + b  // where "+" acts pointwise on vectors a, b of dimension 3
  def addTuple3(a: (Int, Int, Int), b: (Int, Int, Int)): (Int, Int, Int) = {
    (a._1 + b._1, a._2 + b._2, a._3 + b._3)
  }

  //  Pre: none
  // Post: returns s * b  // where "*" is the scalar/vector product
  def mulTuple3(s: Int, a: (Int, Int, Int)): (Int, Int, Int) = {
    (s * a._1, s * a._2, s * a._3)
  }

  def extendedEuclidAlgorithm(m: Int, n: Int): (Int, Int, Int) = {
    require(m > 0 && n > 0)
    var a = (m, 1, 0)
    var b = (n, 0, 1)
    while (b._1 != 0) {
      val q = a._1 / b._1
      val c = addTuple3(a, mulTuple3(-q, b))
      a = b
      b = c
    }
    a
  }

  // And below the same code with a correctness proof:

  //  Pre: m > 0  ∧  n > 0
  // Post: returns (a1, a2, a3)  s.t. a1 == GCD(m, n)  ∧  a1 == m*a2 + n*a3
  def extendedEuclidAlgorithmWithProof(m: Int, n: Int): (Int, Int, Int) = {
    require(m > 0 && n > 0)
    var a = (m, 1, 0)
    var b = (n, 0, 1)
    // Inv: a1 > 0  ∧  b1 >= 0  ∧  GCD(a1, b1) == GCD(m, n)  ∧
    //      a1 == m*a2 + n*a3  ∧  b1 == m*b2 + n*b3
    while (b._1 != 0) {
      // a1 > 0  ∧  b1 > 0  ∧  GCD(a1, b1) == GCD(m, n)  ∧
      //   a1 == m*a2 + n*a3  ∧  b1 == m*b2 + n*b3
      val q = a._1 / b._1
      // a1 > 0  ∧  b1 > 0  ∧  GCD(a1, b1) == GCD(m, n)  ∧
      //   a1 == m*a2 + n*a3  ∧  b1 == m*b2 + n*b3  ∧
      //   q == a._1 / b._1
      val c = addTuple3(a, mulTuple3(-q, b))
      // b1 > 0  ∧  c1 >= 0  ∧  GCD(b1, c1) == GCD(m, n)  ∧
      //   b1 == m*b2 + n*b3  ∧  c1 == m*c2 + n*c3
      a = b
      b = c
      // a1 > 0  ∧  b1 >= 0  ∧  GCD(a1, b1) == GCD(m, n)  ∧
      //   a1 == m*a2 + n*a3  ∧  b1 == m*b2 + n*b3 === Inv
    }
    // a1 > 0  ∧  b1 >= 0  ∧  GCD(a1, b1) == GCD(m, n)  ∧
    //   a1 == m*a2 + n*a3  ∧  b1 == m*b2 + n*b3  ∧  b1 == 0
    // a1 > 0  ∧  GCD(a1, 0) == a1 == GCD(m, n)  ∧
    //   a1 == m*a2 + n*a3
    a
    // returns (a1, a2, a3)  s.t. a1 == GCD(m, n)  ∧  a1 == m*a2 + n*a3
  }

  def main(args: Array[String]): Unit = {
    val (m, n) = (40, 100)
    val (gcd, x, y) = extendedEuclidAlgorithm(m, n)
    println(s"GDC($m, $n) == $gcd == $x * $m  +  $y * $n")
  }
}
