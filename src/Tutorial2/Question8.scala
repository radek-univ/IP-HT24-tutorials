package Tutorial2

import scala.collection.mutable.ArrayBuffer

object Question8 {
  //  Pre: 0 < fraction₀ <= fraction₁
  // Post: returns res: Array[Int] s.t. fraction₀/fraction₁ == ∑_{0 <= i < |res|} 1/res(i)
  def sumOfReciprocals(fraction: (Int, Int)): Array[Int] = {
    var (p, q) = fraction
    require(p > 0, q > p)
    val res = ArrayBuffer[Int]()

    // Inv: fraction₀/fraction₁ == p/q + ∑_{0 <= i < |res|} 1/res(i) ∧ p >= 0 ∧ q > 0
    while (p != 0) {
      val m = ((q - 1) / p) + 1
      res += m
      p = m * p - q
      q = m * q
    }
    res.toArray
  }

  def prepareFraction(fraction: (Int, Int)): (String, String, String) = {
    val (p, q) = fraction
    val (numerator, denominator) = (p.toString, q.toString)
    val l = (numerator.length max denominator.length) + 2
    val center = (s: String) => " " * ((l - s.length + 1) / 2) + s + " " * ((l - s.length) / 2)
    (center(numerator), "-" * l, center(denominator))
  }

  def printReciprocals(a: Array[Int]): Unit = {
    val (t, m, b) = a.map { x => prepareFraction((1, x)) }.unzip3
    println(t.mkString("   ") + "\n" + m.mkString(" + ") + "\n" + b.mkString("   "))
  }

  def printFraction(fraction: (Int, Int)): Unit = {
    val (t, m, b) = prepareFraction(fraction)
    println(s"$t\n$m =\n$b")
  }

  def main(args: Array[String]): Unit = {
    val fraction = (3, 20)
    printFraction(fraction)
    println()
    printReciprocals(sumOfReciprocals(fraction))
  }
}
