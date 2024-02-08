package Tutorial3

object Question3 {
  var X = 23543

  // - Never write this!
  def tooBig(a: Int): Boolean = a > X

  def guess(B: Int): Int = {
    var a = 1
    var b = B
    while (a + 1 < b) {
      val m = (a + b) / 2
      if (tooBig(m))
        b = m
      else
        a = m
    }
    a
  }

  def spinUp2(): Int = {
    var a = 1
    while (!tooBig(2 * a))
      a = 2 * a
    2 * a
  }

  def spinUpK(K: Int): Int = {
    var a = 1
    while (!tooBig(K * a))
      a = K * a
    K * a
  }

  // c)
  // time to find the upper bound: logₖ(x)
  //      size of the upper bound: K(X-1) <= KX
  // binary search time on [0,KX]: log₂(KX)
  //                   total time: logₖX + log₂KX
  //                               == log₂X / log₂K + log₂K + log₂X
  //                               == (1 + 1/log₂K) log₂X + log₂K
  //                               == (1 + ε) log₂X + r

  def guess2(): Int = {
    guess(spinUp2())
    guess(spinUpK(7))
  }
}
