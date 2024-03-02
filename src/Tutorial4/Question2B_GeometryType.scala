package Tutorial4

object Question2B_GeometryType {
  class Triangle {}

  class OpaqueTriangle extends Triangle {}

  trait Renderer {
    type GeometryType
    def acceptForRendering(geometry: GeometryType): Unit
  }

  class RasterisedRenderer extends Renderer {
    override type GeometryType = Triangle
    override def acceptForRendering(geometry: Triangle): Unit = println("Accept for rendering")
  }

  class RayTracingRenderer extends Renderer {
    override type GeometryType = OpaqueTriangle
    override def acceptForRendering(geometry: OpaqueTriangle): Unit = println("Accept for ray-trace rendering")
  }

  def main(args: Array[String]): Unit = {
    val triangle = new Triangle
    val triangle2 = new OpaqueTriangle
    val renderer: RayTracingRenderer = new RayTracingRenderer
    renderer.acceptForRendering(triangle2)
    val renderer2: RasterisedRenderer = new RasterisedRenderer
    renderer2.acceptForRendering(triangle)
    renderer2.acceptForRendering(triangle2)
  }
}
