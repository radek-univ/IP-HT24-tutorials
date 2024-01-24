package Tutorial1

import scala.math.sqrt


object Question1 {
  // a) ----------------------------------------------------
  def square(n: Int): Int = n * n

  def testSquare(): Unit = {
    val v = 5
    // val v = 2147483647 // 1
    // val v = 46341 // ?
    // val v = 2147483641 // 49
    println(s"The result is ${square(v)}.")
  }

  // does not work
  def square2(n: Int): BigInt = n * n

  // may work (please check)
  def square2b(n: Int): BigInt = (n: BigInt) * n

  // will not work only for huge numbers close to BigInt's upper range
  def square2(n: BigInt): BigInt = n * n

  def testSquare2(): Unit = {
    val v = 46341
    println(s"The result for ${v} is ${square2(v)}.")
  }

  // OK
  def square3(n: Int): BigInt = {
    val n2 = BigInt(n)
    n2 * n2
  }

  def testSquare3(): Unit = {
    val v = 46341
    println(s"The result for $v is ${square3(v)}.")
  }

  // b) ----------------------------------------------------
  // Mathematician-friendly modulo operation?
  def mod3v1(x: Int): Int = {
    x % 3
  }

  def mod3v2(x: Int): Int = {
    x - 3 * (x / 3)
  }

  def mod3v3(x: Int): Int = {
    if (x >= 0) x % 3 else (x % 3) + 3
  }

  def mod3v4(x: Int): Int = {
    val res = x % 3
    if (res < 0) res + 3 else res
  }

  def testMod(): Unit = {
    for (v <- -5 to 5) {
      print(f"v = $v%3s: ")
      for (f <- List(mod3v1 _, mod3v2 _, mod3v3 _, mod3v4 _)) {
        val result = f(v)
        print(f"| $result%6s")
      }
      println()
    }
  }

  // c) ----------------------------------------------------
  def biggestSquare(n: Int): Int = {
    require(n >= 0)
    var i: BigInt = 0 // fixed here
    while (i * i <= n) {
      i += 1
      // print(".")
    }
    ((i - 1) * (i - 1)).toInt
  }

  def testBiggestSquare(): Unit = {
    val v = Int.MaxValue // What is the smallest value which causes the loop run forever?
    println(s"biggest square <= ${v} is ${biggestSquare(v)}")
  }

  def biggestSquare2(n: Int): Int = {
    val m = sqrt(n).floor.toInt
    m * m
  }

  def notASuspiciousLoopAtAll(from: Double, to: Double, increment: Double): Unit = {
    require(increment > 0)
    var current = from
    while (current <= to) {
      current += increment;
      print("A")
    }
  }

  def testALoop(): Unit = {
    val increment: Double = 0.000001 // Keep adding zeros both here
    val from: Double = 1
    val to: Double = 1.000005 // and here to observe the dangers of Double
    notASuspiciousLoopAtAll(from, to, increment) // How many letters A will be printed?
  }

  def main(args: Array[String]): Unit = {
    testSquare()
    // testALoop()
    // testBiggestSquare()
    // testALoop()
  }
}
