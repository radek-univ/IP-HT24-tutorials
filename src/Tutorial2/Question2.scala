package Tutorial2

// What will get printed by this program?
object Question2 {
  var x = 3
  var y = 5

  def nasty(x: Int): Int = {
    y = 1
    2 * x
  }

  def main(args: Array[String]): Unit = {
    println(nasty(x) + y)
  }

  println(nasty(y) + y)
}
