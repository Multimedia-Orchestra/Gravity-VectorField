import processing.core._
import org.zhang.lib._

class Gravity extends MyPApplet {

  import PApplet._;
  import PConstants._;

  val mouse = new PVector()
  var pow = 1
  class Dot(val pos: PVector, val vel: PVector = new PVector()) {
    def run() {
      vel.mult(.98f)
      pos.add(vel)
    }

    def push() {
      val dx = pos.x - mouse.x
      val dy = pos.y - mouse.y
      val mag = sqrt(dx*dx + dy*dy)
      vel.add(pow * dx / mag, pow * dy / mag, 0)
    }
  }

  lazy val dots = Array.tabulate(500000)(i => new Dot(new PVector(i * width / 500000f, height/2)))

  override def setup() {
    size(1000, 600)
  }

  override def draw() {
    if(mousePressed) {
      mouse.set(mouseX, mouseY)
      pow = if(mouseButton == LEFT) -1 else 1
      for(d <- dots) d.push()
    }
    background(0)
    loadPixels()
    val cellAlpha = 14

//    println(dots.getClass)
    var i = 0
    while(i < dots.length) {
      val d = dots(i)
      d.run()
      if(!(d.pos.x < 0 || d.pos.y < 0 || d.pos.x >= width || d.pos.y >= height)) {
        val idx = d.pos.y.toInt * width + d.pos.x.toInt
        val b = pixels(idx) & 0xFF
        val nextB = (b + (255 - b) * (cellAlpha / 255f)).toInt
        pixels(idx) = color(nextB)
      }
      i += 1
    }
    updatePixels()
    println(frameRate)
  }
}