package Tutorial1

object Question5 {

  def main(args: Array[String]): Unit = {
    testFib(fib, "fib")
    // testFib(fasterFib, "fasterFib")
    // testFib(fastestFib, "fastestFib")
    // fibPrint(4)
  }

  def fib(n: Int): Int = {
    require(n >= 0)
    if (0 to 1 contains n) {
      n
    } else {
      fib(n - 2) + fib(n - 1)
    }
  }

  def fasterFib(n: Int): BigInt = {
    require(n >= 0)
    if (n == 0)
      return 0

    var (k, x, y) = (1: BigInt, 0: BigInt, 1: BigInt)

    while (k < n) {
      k += 1
      val oldY = y
      y = x + y
      x = oldY
    }
    y
  }

  def testFib(f: Int => BigInt = fastestFib, fName: String = "fastestFib"): Unit = {
    for (n <- 0 to 4500000 by 1) {
      val startTime = System.nanoTime()
      f(n)
      val endTime = System.nanoTime()
      val time = (endTime - startTime) / 1e9d
      println(s"It took $time seconds to compute $fName($n).")
      println("█" * (time * 20).ceil.toInt)
    }
  }

  def fastestFib(n: Int): BigInt = {
    require(n >= 0)
    type Matrix2x2 = (BigInt, BigInt, BigInt, BigInt)

    def mul(A: Matrix2x2, B: Matrix2x2): Matrix2x2 = {
      (
        A._1 * B._1 + A._2 * B._3, A._1 * B._2 + A._2 * B._4,
        A._3 * B._1 + A._4 * B._3, A._3 * B._2 + A._4 * B._4
      )
    }

    var M: Matrix2x2 = (1, 1, 1, 0)
    var res: Matrix2x2 = (1, 0, 0, 1)
    var i = n
    while (i > 0) {
      if (i % 2 == 1) res = mul(res, M)
      M = mul(M, M)
      i /= 2
    }
    res._2
  }

  def fibPrint(n: Int, prefix: String = ""): Int = {
    require(n >= 0)
    println(s"${prefix}fib($n)")
    val prefix2 = prefix + "| "
    val res = if (n <= 1) {
      n
    } else {
      fibPrint(n - 1, prefix2) + fibPrint(n - 2, prefix2)
    }
    println(s"$prefix= $res")
    res
  }
}
