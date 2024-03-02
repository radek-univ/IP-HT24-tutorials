package Tutorial4

object Question1B_FunctionalStyle {
  def main(args: Array[String]): Unit = {
    val shapes = Array(Rectangle(10, 20), Ellipse(30, 40), Square(10), Circle(30), Rectangle(40, 80))
    val shape = Square(30)
    val shape2 = shape.copy(height = 60)
    val shapes2 = (shapes ++ Array(shape2)).map(_.scale(2, 1))
    val squares = shapes2.filter(Square.test)
    println(squares.mkString("Array(", ", ", ")"))
  }

  // Functional programming style
  trait Shape {
    def scale(x: Int, y: Int): Shape
  }

  case class Rectangle(width: Int, height: Int) extends Shape {
    override def scale(x: Int, y: Int): Shape = Rectangle(width * x, height * y)
  }

  case class Ellipse(minor: Int, major: Int) extends Shape {
    override def scale(x: Int, y: Int): Shape = Ellipse(minor * x, major * y)
  }

  object Circle {
    def apply(radius: Int): Ellipse = Ellipse(radius, radius)

    def test(s: Shape): Boolean = s match {
      case Ellipse(minor, major) if minor == major => true
      case _                                       => false
    }
  }

  object Square {
    def apply(side: Int): Rectangle = Rectangle(side, side)

    def test(s: Shape): Boolean = s match {
      case Rectangle(w, h) if w == h => true
      case _                         => false
    }
  }
}
