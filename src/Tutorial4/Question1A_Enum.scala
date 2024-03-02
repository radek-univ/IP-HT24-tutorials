package Tutorial4

object Question1A_Enum {
  // Variant of a solution I have seen
  // NOT IN THE SPIRIT OF OBJECT-ORIENTED PROGRAMMING!
  def main(args: Array[String]): Unit = {
    val shapes = Array(Square(10), Circle(20), Rectangle(30, 30), Ellipse(40, 50))
    val squares = shapes.filter(Square.check)
    println(squares.mkString("Array(", ", ", ")"))
  }

  import Shape.ShapeType

  case class Shape(var len: Int, var wid: Int, var typ: ShapeType.T) {
    def resize(nlen: Int, nwid: Int): Unit = {
      len = nlen
      wid = nwid
    }
    def isSquare: Boolean = len == wid && typ == ShapeType.Rectangle
    def isCircle: Boolean = len == wid && typ == ShapeType.Ellipse
  }

  object Shape {
    object ShapeType extends Enumeration {
      type T = Value
      val Ellipse, Rectangle = Value
    }
  }

  object Rectangle {
    def apply(width: Int, height: Int): Shape = new Shape(width, height, ShapeType.Rectangle)
  }

  object Ellipse {
    def apply(semiMinor: Int, semiMajor: Int): Shape = new Shape(semiMinor, semiMajor, ShapeType.Ellipse)
  }

  object Circle {
    def apply(radius: Int): Shape = Ellipse(radius, radius)
  }

  object Square {
    def apply(side: Int): Shape = Rectangle(side, side)

    def check(shape: Shape): Boolean = shape.typ == ShapeType.Rectangle && shape.wid == shape.len
  }
}
