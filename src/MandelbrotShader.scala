import org.zhang.geom.Vec2
import processing.core._
import org.zhang.lib._

class MandelbrotShader extends MyPApplet {

  import PApplet._;
  import PConstants._;
  import zhang.Camera

  lazy val mandelShader = loadShader("mandelbrot.glsl")

  var topLeft = Vec2(-2, -2)
  var bottomRight = Vec2(2, 2)
  
  override def setup() {
    size(800, 800, P2D)
    mandelShader.set("resolution", width.toFloat, height.toFloat)
    background(0)
  }
  
  def pan(offset: Vec2) {
    topLeft += offset
    bottomRight += offset
  }
  
  def zoomIn(amount: Float) {
    val center = (topLeft + bottomRight) / 2
    val oldDist = topLeft - center
    val newDist = oldDist / amount
    topLeft = center + newDist
    bottomRight = center - newDist
  }
  
  def worldDimensions = bottomRight - topLeft

  override def draw() {
    if(mousePressed) {
      val screenOffset = mouseVec - Vec2(pmouseX, pmouseY)
      val worldOffset = Vec2(+map(screenOffset.x, 0, width, 0, worldDimensions.x),
                             -map(screenOffset.y, 0, height, 0, worldDimensions.y))
      pan(-worldOffset)
    }
    if(keyPressed) {
      key match {
        case '-' | '_' => zoomIn(1 / 1.03f)
        case '=' | '+' => zoomIn(1.03f)
        case _ => {}
      }
    }
    mandelShader.set("topLeft", topLeft.x, topLeft.y)
    mandelShader.set("bottomRight", bottomRight.x, bottomRight.y)
    shader(mandelShader)
    rect(0, 0, width, height)
    println(frameRate)
  }
}