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

    private var prunedFingers = List[Finger]()

    def update() {
      val incoming = leap.getFingers
        .filter{_.getPosition.z != 50.0f} // There's sometimes erroneously reported fingers at z exactly 50.0
        .take(10).toList

//      incomingHistory = (incoming :: incomingHistory).take(5)
//
//      // only keep fingers that have existed for the past 5 frames
//      incomingHistory.reduceLeft((remaining, frame) => remaining.filter(_.getId))

      prunedFingers = incoming.filter(_.getTimeVisible > .3f)
    }

    //returns a list of 0, 1, or 2 Fingers
    def getFingers = prunedFingers
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
      rectMode(CORNER)
      rect(0, 0, width, height)

      textAlign(CENTER, CENTER)
      fill(255, alpha)
      textSize(16)
      rectMode(CENTER)
      text("Steadily hold your finger at least 12 inches above the Leap Motion.", width/3, height/4, 300, 80)

      val yBaseline = 2 * height / 3
      tint(255, alpha)
      image(leapMotion, width/3, yBaseline)
      textAlign(LEFT)
      text("12 in", width/3 + 35, yBaseline - leapMotion.height / 4)
      val leapMotionTop = yBaseline - leapMotion.height/2
      image(hand, width/3 - hand.width/2 + 50 * logistic(sin(millis() / 400f)), leapMotionTop - 25 + 50 * logistic(sin(millis() / 350f)))

      def drawAttractHelp(x: Float) {
        fill(255, alpha)
        textAlign(CENTER, CENTER)
        rectMode(CENTER)
        text("Poke your finger past the Leap Motion to attract particles.", 2*width/3, height/4, 300, 80)
        image(leapMotionOverhead, x, yBaseline)
        imageMode(CORNER)
        val fingerZ = map(logistic(4 * sin(millis() / 500f)), -1, 1, 0, 100)
        // the center of the pointing finger is about 15, 15 so add that to the offset to "center"
        // the image on the finger
        // add 30 to the y to move the finger to the back of the leap motion
        image(handOverhead, x - 15, yBaseline - 15 + 60 - fingerZ)
        imageMode(CENTER)
        drawAttractor(fingerZ, x, yBaseline - 100)
        if(fingerZ > 50) {
          fill(64, 255, 128, alpha)
          text("Attracting", x + 150, yBaseline - 100)
        }
      }
      drawAttractHelp(2*width/3)
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


  lazy val attract = loadImage("attract.png")
  lazy val repel = loadImage("repel.png")
  lazy val leapMotionOverhead = loadImage("leapmotion-over.png")
  lazy val handOverhead = loadImage("hand-with-finger-over.png")
  lazy val leapMotion = loadImage("leapmotion.png")
  lazy val hand = loadImage("hand-with-finger.png")
//  lazy val motionDirections = loadImage("motion-directions.png")
  // When emptyCount >= this threshold, show the help message
  val SHOW_HELP_THRESHOLD = 800

  var emptyCount = 0
  override def setup() {
    size(displayWidth, displayHeight)
    imageMode(CENTER)
    reset()
    attract.resize(150, 0)
    repel.resize(150, 0)
    leapMotion.resize(0, 300)
    frameRate(25)
  }

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
    Tracker.update()
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
