import processing.core._
import org.zhang.lib._

class Gravity extends MyPApplet {

  import PApplet._
  import PConstants._

  import Vec2Conversion._

  object Tracker {
    import de.voidplus.leapmotion._
    import scala.collection.JavaConversions._

    lazy val leap = new LeapMotion(Gravity.this)

    //returns a list of 0, 1, or 2 Fingers
    def getHands = {
      //get the leftmost and rightmost fingers as the two points
      leap.countHands() match {
        case 0 => List()
        case 1 => List(leap.getHands.get(0).getFrontFinger)
        case _ => List(leap.getHands.get(0).getFrontFinger, leap.getHands.get(1).getFrontFinger)
      }
    }
  }

  val p1 = new PVector()
  val p2 = new PVector()
  class Dot(val pos: PVector, val vel: PVector = new PVector()) {
    def attract(vec: PVector) {
      val dx = pos.x - vec.x
      val dy = pos.y - vec.y
      val mag = sqrt(dx*dx + dy*dy)
      vel.add(vec.z * dx / mag, vec.z * dy / mag, 0)
    }
    def run() {
      if(p1.z != 0)
        attract(p1)
      if(p2.z != 0)
        attract(p2)

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

  def reset() {
    for(i <- 0 until dots.length) {
      dots(i).vel.set(0, 0)
      dots(i).pos.set(i * width / 1000000f, height/2)
    }
  }

  lazy val dots = Array.tabulate(1000000)(i => new Dot(new PVector(i * width / 1000000f, height/2)))
  lazy val parDots = dots.par

  val cellAlpha = 14
  lazy val colorCache = Array.tabulate(256)(b => color((b + (255 - b) * (cellAlpha / 255f)).toInt))

  var emptyCount = 0
  override def setup() {
    size(displayWidth, displayHeight)
  }

  override def draw() {
    background(0)

    val hands = Tracker.getHands
    if(hands.isEmpty) emptyCount += 1
    else emptyCount = 0
    if(emptyCount == 1200) {
      reset()
    }
    hands.headOption.
      map{f =>
        val amount = constrain(sqrt(map(f.getTouchDistance, 0, 1, 1, 0)), .001f, .98f)
        fill(255, 245, 245, 2)
        noStroke()
        var radius = 100f
        while(radius > 2) {
          ellipse(f.getPosition.x, f.getPosition.y, radius, radius)
          radius *= amount
        }
        f
      }.filter(_.getTouchDistance < 0).map{f => p1.set(f.getPosition.x, f.getPosition.y, -1)}.getOrElse(p1.z = 0)
    hands.drop(1).headOption.
      map{f =>
        val amount = constrain(sqrt(map(f.getTouchDistance, 0, 1, 1, 0)), .001f, .98f)
        noStroke()
        var radius = 100f
        while(radius > 2) {
          fill(lerpColor(color(0), color(245, 245, 255, 2), radius / 100f))
          ellipse(f.getPosition.x, f.getPosition.y, radius, radius)
          radius *= amount
        }
        f
      }.filter(_.getTouchDistance < 0).map{f => p2.set(f.getPosition.x, f.getPosition.y, .95f)}.getOrElse(p2.z = 0)

    loadPixels()
//    if(mousePressed) {
//      p1.set(mouseX, mouseY, if(mouseButton == LEFT) -1 else 1)
//    } else {
//      p1.z = 0
//    }
//    p2.set(width/2, height/2, -1)

    parDots.foreach(_.run())
    updatePixels()
    println(frameRate)
  }
}