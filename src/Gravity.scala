import de.voidplus.leapmotion.Finger
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
    def runAndDraw(g: PGraphics) {
      if(p1.z != 0)
        attract(p1)
      if(p2.z != 0)
        attract(p2)

      vel.mult(.98f)
      pos.add(vel)
      draw(g)
    }

    def draw(g: PGraphics) {
      if(!(pos.x < 0 || pos.y < 0 || pos.x >= g.width || pos.y >= g.height)) {
        val idx = pos.y.toInt * g.width + pos.x.toInt
        //this line has a non-deterministic set in it but it doesn't make a difference visually
        g.pixels(idx) = colorCache(g.pixels(idx) & 0xFF) // get the blue component (which is the same as red, and green, and the brightness)
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
  lazy val dustImage = createGraphics(width, height, JAVA2D)

  // When emptyCount >= this threshold, show the help message
  val SHOW_HELP_THRESHOLD = 400

  var emptyCount = SHOW_HELP_THRESHOLD
  var lastFrameWithoutFingers = 0
  override def setup() {
    size(1024, 768)
    imageMode(CENTER)
  }

  lazy val attract = loadImage("attract.png")
  lazy val repel = loadImage("repel.png")
  lazy val leapMotion = loadImage("leapmotion-over.png")
  lazy val handWithFinger = loadImage("hand-with-finger-over.png")
  lazy val motionDirections = loadImage("motion-directions.png")

  def drawDust(g: PGraphics, hands: List[Finger]) {
    g.beginDraw()
    g.background(0)
    g.imageMode(CENTER)

    hands.headOption.
      map{f =>
        val amount = pow(constrain(map(f.getPosition.z, 50, 0, 1f, 0f), 0, 1f), 2.7f)
        val tintC = if(amount < .998f) 128 else 255
        g.tint(tintC, amount * 255)
        g.image(attract, f.getPosition.x, f.getPosition.y)
        f
      }.filter(_.getPosition.z > 50).map{f => p1.set(f.getPosition.x, f.getPosition.y, -(1+f.getVelocity.mag() / 1000f))}.getOrElse(p1.z = 0)
    hands.drop(1).headOption.
      map{f =>
        val amount = pow(constrain(map(f.getPosition.z, 50, 0, 1f, 0f), 0, 1f), 2.7f)
        val tintC = if(amount < .998f) 128 else 255
        g.tint(tintC, amount * 255)
        g.image(repel, f.getPosition.x, f.getPosition.y)
        f
      }.filter(_.getPosition.z > 50).map{f => p2.set(f.getPosition.x, f.getPosition.y, (.95f + f.getVelocity.mag() / 1000f))}.getOrElse(p2.z = 0)

    g.loadPixels()
    parDots.foreach(_.runAndDraw(g))
    g.updatePixels()

    g.endDraw()
  }

  def drawInstructions(alpha: Float) {
    pushStyle()
    fill(255, alpha / 2)
    rect(0, 0, width, height)

    textAlign(CENTER, CENTER)
    fill(255, alpha)
    text("Move your finger up and down, side to side.", width/2, height/4)
    tint(255, alpha)
    image(leapMotion, width/4, height/2)
    imageMode(CORNER)
    image(handWithFinger, width/4 + 80 * logistic(sin(millis() / 500f)), height/2 + 30)
    imageMode(CENTER)

    image(leapMotion, 2*width/4, height/2)
    matrix {
      translate(width/2 + handWithFinger.width / 2 - 15, height/2 + handWithFinger.height / 2 - 15 + 30)
      scale(pow(1.25f, logistic(sin(millis() / 300f))))
      image(handWithFinger, 0, 0)
    }

    image(leapMotion, 3*width/4, height/2)
    imageMode(CORNER)
    val pokeOffset = -(1 + logistic(4 * sin(millis() / 500f))) / 2 * 50
    // the center of the pointing finger is about 15, 15 so add that to the offset to "center"
    // the image on the finger
    // add 30 to the y to move the finger to the back of the leap motion
    image(handWithFinger, 3*width/4 - 15, height/2 - 15 + 30 + pokeOffset)
    imageMode(CENTER)
    popStyle()
  }

  def logistic(x: Float) = 2 * (1 / (1 + exp(-x * PI)) - .5f)

  override def draw() {
//    if(mousePressed) {
//      p1.set(mouseX, mouseY, if(mouseButton == LEFT) -1 else 1)
//    } else {
//      p1.z = 0
//    }
//    p2.set(width/2, height/2, -1)

    val hands = Tracker.getHands
    drawDust(dustImage, hands)
    image(dustImage, width/2, height/2)
    
    if(hands.isEmpty) {
      emptyCount += 1
      lastFrameWithoutFingers = frameCount
      
      if(emptyCount > SHOW_HELP_THRESHOLD) {
        val alpha = min(255, (emptyCount - SHOW_HELP_THRESHOLD) * 5)
        drawInstructions(alpha)
      }
      if(emptyCount == Gravity.EMPTY_COUNT) {
        reset()
      }
      
    } else {
      emptyCount = 0
      val alpha = max(0, 255 - (frameCount - lastFrameWithoutFingers) * 15)
      if(alpha > 0) {
        drawInstructions(alpha)
      }
    }
    println(hands)

    if(keyPressed && key == ' ') {
      saveFrame("frames/gravity-####.png")
    }
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
    PApplet.main(Array("--display=1", "Gravity"))
  }
}
