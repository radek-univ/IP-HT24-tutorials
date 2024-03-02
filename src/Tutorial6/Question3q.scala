package Tutorial6

object Question3q {
  class Record(var word: String, var count: Int)

  def main(args: Array[String]): Unit = {
    val array = Array.fill[Record](5)(new Record(null, 0))

    val array2 = Array.fill[Record](5) {
      new Record(null, 0)
    } // This is the same, but looks less surprising with the curly brackets.

    array(0).word = f"ABC"

    // What will be printed? Why?
    println(array(4).word)
  }
}
