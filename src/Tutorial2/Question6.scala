package Tutorial2

import org.scalatest.funsuite.AnyFunSuite

// Predicate 'period':
// hasPeriodPred(s, i) === s[0..s.length-i) == s[i..|s|)
class Question6 extends AnyFunSuite {
  //  Pre: s.length > 0
  // Post: returns i s.t. hasPeriod(s, i)  ∧  ∀j∈[1..i). ¬hasPeriod(s, j)
  def period(s: Array[Char]): Int = {
    var p = 1
    // Inv: ∀j∈[1..i). ¬hasPeriod(s, j)  ∧  p <= |s|
    while (p < s.length && !hasPeriod(s, p))
      p += 1
    p
  }

  // Pre: 0 < p <= |s|
  // Post: returns hasPeriodPred(s, p)
  def hasPeriod(s: Array[Char], p: Int): Boolean = {
    var i = 0
    // I: hasPeriod(s[0..i+p), p)  ∧  0 < i < i+p <= |s|
    while (i + p < s.length && s(i) == s(i + p))
      i += 1
    i + p == s.length
  }

  test("minRecurrencePeriodTest") {
    val testCases = Array(
      (" b bb b bb b bb b ", 5),
      (" b bbb b bb b bbb b ", 11),
      (" xx x x  x xx x x  x xx x ", 10),
      ("  x x  x  x x  x  x x  x  x ", 8),
      ("  x x  ", 5),
    )
    for ((s, expected) <- testCases) {
      val actual = period(s.toArray)
      assert(
        actual === expected,
        s"\nFunction minRecurrencePeriod(\"$s\".toArray) should return $expected, got $actual."
      )
    }
  }
}
