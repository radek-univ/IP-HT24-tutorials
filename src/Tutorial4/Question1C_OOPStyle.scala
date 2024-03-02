package Tutorial4

object Question1C_OOPStyle {
  // OOP style solution
  def main(args: Array[String]): Unit = {
    val shapes = Array[Shape](Rectangle(10, 20), Ellipse(30, 40), Square(10), Circle(30), Rectangle(40, 80))
    shapes.foreach(_.scale(2, 1))
    val squares = shapes.filter(_.isSquare)
    println(squares.mkString("Array(", ", ", ")"))
  }

  abstract class Shape(protected var dim1: Int, protected var dim2: Int) {
    override def toString: String = s"${this.getClass.getSimpleName}($dim1, $dim2)"
    def scale(cx: Int, cy: Int): Unit = {
      dim1 *= cx
      dim2 *= cy
    }
    def isSquare: Boolean = false
    def isCircle: Boolean = false
  }

  class Rectangle(width: Int, height: Int) extends Shape(width, height) {
    override def isSquare: Boolean = dim1 == dim2
    def width: Int = dim1
    def width_=(v: Int): Unit = dim1 = v
    def height: Int = dim2
    def height_=(v: Int): Unit = dim2 = v
  }

  class Ellipse(minor: Int, major: Int) extends Shape(minor, major) {
    override def isCircle: Boolean = dim1 == dim2
    def minor: Int = dim1
    def minor_=(v: Int): Unit = dim1 = v
    def major: Int = dim2
    def major_=(v: Int): Unit = dim2 = v
  }

  object Rectangle {
    def apply(width: Int, height: Int): Rectangle = new Rectangle(width, height)
  }

  object Square {
    def apply(side: Int): Rectangle = Rectangle(side, side)
  }

  object Ellipse {
    def apply(minor: Int, major: Int): Ellipse = new Ellipse(minor, major)
  }

  object Circle {
    def apply(radius: Int): Ellipse = Ellipse(radius, radius)
  }
}
