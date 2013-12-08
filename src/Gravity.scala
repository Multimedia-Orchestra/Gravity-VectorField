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
      def minFinger(h: Option[Hand]) = h.map{_.getFingers.filter{_.getPosition.z != 50.0f}.sortBy{_.getPosition.z}.headOption}.flatten.toList
      leap.countHands() match {
        case 0 => List()
        case 1 => minFinger(leap.getHands.headOption)
        case _ => List(minFinger(leap.getHands.headOption), minFinger(leap.getHands.drop(1).headOption)).flatten
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
        //this line has a non-deterministic set in it but it doesn't make a difference visually
        pixels(idx) = colorCache(pixels(idx) & 0xFF) // get the blue component (which is the same as red, and green, and the brightness)
      }
    }
  }

  def reset() {
    for(i <- 0 until dots.length) {
      dots(i).vel.set(0, 0)
      dots(i).pos.set(i.toFloat * width / Gravity.NUM, height/2)
    }
  }

  lazy val dots = Array.tabulate(Gravity.NUM)(i => new Dot(new PVector(i.toFloat * width / Gravity.NUM, height/2)))
  lazy val parDots = dots.par

  lazy val colorCache = Array.tabulate(256)(b => color((b + (255 - b) * (Gravity.CELL_ALPHA / 255f)).toInt))

  var emptyCount = 0
  override def setup() {
    size(displayWidth, displayHeight)
    imageMode(CENTER)
  }

  lazy val attract = loadImage("attract.png")
  lazy val repel = loadImage("repel.png")

  override def draw() {
    background(0)

    val hands = Tracker.getHands
    if(hands.isEmpty) emptyCount += 1
    else emptyCount = 0
    if(emptyCount == Gravity.EMPTY_COUNT) {
      reset()
    }
    println(hands)
    hands.headOption.
      map{f =>
        val amount = pow(constrain(map(f.getPosition.z, 50, 0, 1f, 0f), 0, 1f), 2.7f)
        val tintC = if(amount < .998f) 128 else 255
        tint(tintC, amount * 255)
        image(attract, f.getPosition.x, f.getPosition.y)
        f
      }.filter(_.getPosition.z > 50).map{f => p1.set(f.getPosition.x, f.getPosition.y, -(1+f.getVelocity.mag() / 1000f))}.getOrElse(p1.z = 0)
    hands.drop(1).headOption.
      map{f =>
        val amount = pow(constrain(map(f.getPosition.z, 50, 0, 1f, 0f), 0, 1f), 2.7f)
        val tintC = if(amount < .998f) 128 else 255
        tint(tintC, amount * 255)
        image(repel, f.getPosition.x, f.getPosition.y)
        f
      }.filter(_.getPosition.z > 50).map{f => p2.set(f.getPosition.x, f.getPosition.y, (.95f + f.getVelocity.mag() / 1000f))}.getOrElse(p2.z = 0)

    loadPixels()
//    if(mousePressed) {
//      p1.set(mouseX, mouseY, if(mouseButton == LEFT) -1 else 1)
//    } else {
//      p1.z = 0
//    }
//    p2.set(width/2, height/2, -1)

    parDots.foreach(_.run())
    updatePixels()
//    println(frameRate)
  }
}

object Gravity {
  var NUM: Int = _
  var CELL_ALPHA: Int = _
  var EMPTY_COUNT: Int = _
  def main(args: Array[String]) {
    NUM = args.headOption.map{_.toInt}.getOrElse(250000)
    CELL_ALPHA = args.drop(1).headOption.map{_.toInt}.getOrElse(14)
    EMPTY_COUNT = args.drop(2).headOption.map{_.toInt}.getOrElse(1200)
    PApplet.main(Array("--present", "--display=1", "Gravity"))
  }
}