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
    def getFingers =
      leap.getFingers
        .filter{_.getPosition.z != 50.0f}
        .take(10).toList
  }

  class Dot(val pos: PVector, val vel: PVector = new PVector()) {
    def attract(vec: PVector) {
      val dx = pos.x - vec.x
      val dy = pos.y - vec.y
      val mag = sqrt(dx*dx + dy*dy)
      vel.add(vec.z * dx / mag, vec.z * dy / mag, 0)
    }

    def runAndDraw0() {
      step2()
    }
    def runAndDraw1() {
      if(attractors(0).z != 0)
        attract(attractors(0))
      step2()
    }

    def runAndDraw2() {
      if(attractors(0).z != 0)
        attract(attractors(0))
      if(attractors(1).z != 0)
        attract(attractors(1))
      step2()
    }
    def runAndDraw3() {
      if(attractors(0).z != 0)
        attract(attractors(0))
      if(attractors(1).z != 0)
        attract(attractors(1))
      if(attractors(2).z != 0)
        attract(attractors(2))
      if(attractors(3).z != 0)
        attract(attractors(3))
      step2()
    }
    def runAndDraw4() {
      if(attractors(0).z != 0)
        attract(attractors(0))
      if(attractors(1).z != 0)
        attract(attractors(1))
      if(attractors(2).z != 0)
        attract(attractors(2))
      if(attractors(3).z != 0)
        attract(attractors(3))
      if(attractors(4).z != 0)
        attract(attractors(4))

      step2()
    }
    
    def step2() {
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
    emptyCount = SHOW_HELP_THRESHOLD
  }

  object Instructions {
    private var alpha = 0.0f
    private var wantedAlpha = 0.0f

    def show() {
      wantedAlpha = 255
    }

    def hide() {
      wantedAlpha = 0
    }

    private def drawInstructions(alpha: Float) {
      pushStyle()
      // background color
      fill(128, alpha / 2)
      rect(0, 0, width, height)

      textAlign(CENTER, CENTER)
      fill(255, alpha)
      textSize(20)
      val leftWidth = textWidth("Keep your hand at least ")
      val rightWidth = textWidth("one foot above the Leap Motion.")
      text("Keep your hand at least ", width/2 - rightWidth/2, height/4)
      fill(64, 255, 128, alpha)
      text("one foot above the Leap Motion.", width/2 + leftWidth/2, height/4)

      val yBaseline = 2 * height / 3
      tint(255, alpha)
      image(leapMotion, width/4, yBaseline)
      imageMode(CORNER)
      image(handWithFinger, width/4 + 80 * logistic(sin(millis() / 500f)), yBaseline + 30)
      imageMode(CENTER)

      image(leapMotion, 2*width/4, yBaseline)
      matrix {
        translate(width/2 + handWithFinger.width / 2 - 15, yBaseline + handWithFinger.height / 2 - 15 + 30)
        scale(pow(1.1f, logistic(sin(millis() / 300f))))
        image(handWithFinger, 0, 0)
      }

      image(leapMotion, 3*width/4, yBaseline)
      imageMode(CORNER)
      val fingerZ = map(logistic(4 * sin(millis() / 500f)), -1, 1, 0, 100)
      // the center of the pointing finger is about 15, 15 so add that to the offset to "center"
      // the image on the finger
      // add 30 to the y to move the finger to the back of the leap motion
      image(handWithFinger, 3*width/4 - 15, yBaseline - 15 + 60 - fingerZ)
      imageMode(CENTER)
      drawAttractor(fingerZ, 3*width/4, yBaseline - 100)
      if(fingerZ > 50) {
        fill(64, 255, 128, alpha)
        text("Attracting", 3*width/4 + 150, yBaseline - 100)
      }
      popStyle()
    }

    def stepAndDraw() {
      alpha = alpha * .75f + wantedAlpha * .25f
      if(alpha < 5) {
        alpha = wantedAlpha
      } else if(alpha > 254) {
        alpha = wantedAlpha
      }

      if(alpha >= 5) {
        drawInstructions(alpha)
      }
    }
  }

  lazy val dots = Array.tabulate(Gravity.NUM)(i => new Dot(new PVector(i.toFloat * width / Gravity.NUM, height/2)))
  lazy val parDots = dots.par
  val attractors = Array.fill(10)(new PVector())

  lazy val colorCache = Array.tabulate(256)(b => color((b + (255 - b) * (Gravity.CELL_ALPHA / 255f)).toInt))

  // [0] == this frame, [1] == 1 frame ago, etc...
  var hasFingerHistory = List[Boolean]()

  // When emptyCount >= this threshold, show the help message
  val SHOW_HELP_THRESHOLD = 800

  var emptyCount = 0
  override def setup() {
    size(displayWidth, displayHeight)
    imageMode(CENTER)
    reset()
    attract.resize(150, 0)
    repel.resize(150, 0)
    frameRate(25)
  }

  lazy val attract = loadImage("attract.png")
  lazy val repel = loadImage("repel.png")
  lazy val leapMotion = loadImage("leapmotion-over.png")
  lazy val handWithFinger = loadImage("hand-with-finger-over.png")
  lazy val motionDirections = loadImage("motion-directions.png")

  def drawAttractor(fingerZ: Float, x: Float, y: Float) {
    val amount = pow(constrain(map(fingerZ, 50, 0, 1f, 0f), 0, 1f), 2.7f)
    val tintC = if(amount < .998f) 128 else 255
    tint(tintC, amount * 255)
    image(attract, x, y)
  }

  def runAndDrawDust(fingers: List[Finger]) {
    background(0)
    imageMode(CENTER)

    attractors.foreach {_.z = 0}
    for((finger, idx) <- fingers.zipWithIndex) {
      drawAttractor(finger.getPosition.z, finger.getPosition.x, finger.getPosition.y)
      if(finger.getPosition.z > 50) {
        attractors(idx).set(finger.getPosition.x, finger.getPosition.y, -(1+finger.getVelocity.mag() / 1000f))
      }
    }

    loadPixels()
    fingers.length match {
      case 0 => parDots.foreach(_.runAndDraw0())
      case 1 => parDots.foreach(_.runAndDraw1())
      case 2 => parDots.foreach(_.runAndDraw2())
      case 3 => parDots.foreach(_.runAndDraw3())
      case 4 | _ => parDots.foreach(_.runAndDraw4())
    }
    updatePixels()
  }

  def logistic(x: Float) = 2 * (1 / (1 + exp(-x * PI)) - .5f)


  override def draw() {
    val fingers = Tracker.getFingers
    runAndDrawDust(fingers)
    
    if(fingers.isEmpty) {
      emptyCount += 1
      if(emptyCount > SHOW_HELP_THRESHOLD) {
        Instructions.show()
      }
      if(emptyCount >= Gravity.EMPTY_COUNT) {
        reset()
      }
    } else {
      emptyCount = 0
      Instructions.hide()
    }
    Instructions.stepAndDraw()
//    println(hands)

    if(keyPressed && key == ' ') {
      saveFrame("frames/gravity-####.png")
    }
    println(frameRate)
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
    PApplet.main(Array("--display=1", "--present", "Gravity"))
  }
}
