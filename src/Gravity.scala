import processing.core._
import org.zhang.lib._

class Gravity extends MyPApplet {

  import PApplet._
  import PConstants._

  val mouse = new PVector()
  var pow = 1
  class Dot(val pos: PVector, val vel: PVector = new PVector()) {
    def run() {
      if(mousePressed) {
        val dx = pos.x - mouse.x
        val dy = pos.y - mouse.y
        val mag = sqrt(dx*dx + dy*dy)
        vel.add(pow * dx / mag, pow * dy / mag, 0)
      }

      vel.mult(.98f)
      pos.add(vel)
      draw()
    }

    def draw() {
      if(!(pos.x < 0 || pos.y < 0 || pos.x >= width || pos.y >= height)) {
        val idx = pos.y.toInt * width + pos.x.toInt
        //this line has a non-deterministic condition in it but it doesn't make a difference visually
        pixels(idx) = colorCache(pixels(idx) & 0xFF) // get the blue component (which is the same as red, and green, and the brightness)
      }
    }
  }

  lazy val dots = Array.tabulate(1000000)(i => new Dot(new PVector(i * width / 1000000f, height/2)))
  lazy val parDots = dots.par

  val cellAlpha = 14
  lazy val colorCache = Array.tabulate(255)(b => color((b + (255 - b) * (cellAlpha / 255f)).toInt))

  override def setup() {
    size(displayWidth, displayHeight)
  }

  override def draw() {
    background(0)
    loadPixels()
    if(mousePressed) {
      mouse.set(mouseX, mouseY)
      pow = if(mouseButton == LEFT) -1 else 1
    }
    parDots.foreach(_.run())
    updatePixels()
    println(frameRate)
  }
}