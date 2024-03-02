package Tutorial4

object Question2A_ToFix {
  class Triangle
  class OpaqueTriangle extends Triangle

  class Renderer {
    def accept(a: Triangle): Unit = println("Accepted for rendering.")
  }

  class RayTracingRenderer extends Renderer {
    // Fix this:
    // def accept(a: OpaqueTriangle): Unit = println("Accepted for ray-trace rendering.")
    override def accept(a: Triangle): Unit = println("Accepted for ray-trace rendering.")
    // Question: Is this really a fix? Is RayTracingRenderer a special case of Renderer?
    // It does not satisfy the Liskov substitution principle:
    // If we substitute a Renderer with a RayTracingRenderer,
    // it stops accepting Triangles which are not OpaqueTriangles.
  }

  def main(args: Array[String]): Unit = {
    val a: OpaqueTriangle = new OpaqueTriangle
    val r1: Renderer = new RayTracingRenderer
    r1.accept(a)
    val r2: RayTracingRenderer = new RayTracingRenderer
    r2.accept(a)
  }
}
