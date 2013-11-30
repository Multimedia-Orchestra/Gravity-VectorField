import org.zhang.geom.Vec2
import processing.core._
import org.zhang.lib.MyPApplet
import org.zhang.lib._

class Main extends MyPApplet {

  import PApplet._;
  import PConstants._;

  val NUM_CELLS = 70
  lazy val dimensions = Vec2(width, height)

  override def setup() {
    size(displayWidth, displayHeight, P2D)
    noSmooth()
  }

  def profile[A](name:String)(m: => A) {
    profileMap(name) += time(m)._2
  }
  def resetProfiler() {
    profileMap.foreach { case (k, v) => profileMap(k) = 0 }
  }
  def profileReport = {
    val total = profileMap.values.sum
    profileMap.map{ case (k, v) => k+": "+v.toDouble / total }.mkString("\n")
  }
  val profileMap = collection.mutable.Map[String, Long]().withDefaultValue(0)

  class Cell(val x: Int, val y: Int, var vec2:Vec2 = Vec2()) {
    val pos = Vec2(map(x+.5f, 0, NUM_CELLS, 0, width), map(y+.5f, 0, NUM_CELLS, 0, height))

    var balls = collection.mutable.ArrayBuffer[Ball]()
    var nextVec2 = vec2
    def step() {
      //seed
      nextVec2 += Vec2(noise(pos.x / 300, pos.y / 300, millis() / 3000f) - .5f, noise(pos.y / 270, pos.x / 420 + 14, millis() / 3000f) - .5f)

      val neighbors =
        for(v <- List(Vec2(-1, 0), Vec2(1, 0), Vec2(0, -1), Vec2(0, 1)).map{_ + Vec2(x, y)})
        yield vectorField(wrapIndex(v.x.toInt))(wrapIndex(v.y.toInt))

      //equal diffusion
      val diffuEqual = neighbors.map{_.vec2}.reduce(_ + _) / 4

      //directed diffusion
      val diffuDirected = neighbors.map{ c =>
        val direction = (pos - c.pos).normalize
        val mag = direction dot c.vec2
        val proportion = max(0, if(c.vec2.mag == 0) 0 else mag / c.vec2.mag2) //raise c.vec2.mag by a power
        val contribution = c.vec2 * proportion
        contribution
      }.reduce(_ + _) //divide by a scalar

      val diffusion = diffuEqual * .2f + diffuDirected * .8f

      val stasis = 0.5f //somewhere between 0 and 1
      nextVec2 = nextVec2 * stasis + diffusion * (1 - stasis)

//      if(nextVec2.mag > 50) nextVec2 = Vec2()


    }
    def updateAndDraw() {
      val offset = nextVec2 - vec2
      vec2 = nextVec2
      balls.clear()

//      stroke(offset.mag * 128)
      stroke(vec2.mag * 40)
//      line(pos, pos + vec2.normalize * pow(vec2.mag, 1 / 4f))
      line(pos, pos + vec2 * 20)
    }
  }

  class Ball(var pos: Vec2) {
    def cell = cellAt(pos)
    def step() {
      pos += Vec2.random(cell.balls.length / 10f)
      cell.balls += this
      pos = Vec2(wrapWorldX(pos.x + cell.vec2.x), wrapWorldY(pos.y + cell.vec2.y))
    }
  }

  def wrapIndex(x:Int) = (x + NUM_CELLS) % NUM_CELLS
  def wrapWorldX(x:Float) = (x + width) % width
  def wrapWorldY(x:Float) = (x + height) % height

  //world position into indexed position
  def cellAt(pos: Vec2) = vectorField(wrapIndex(map(pos.x, 0, width, 0, NUM_CELLS).toInt))(wrapIndex(map(pos.y, 0, height, 0, NUM_CELLS).toInt))

  lazy val vectorField = collection.mutable.Seq.tabulate(NUM_CELLS, NUM_CELLS)((x, y) => new Cell(x, y))
  lazy val balls = collection.mutable.Seq.fill(20000){ new Ball(Vec2(random(width), random(height))) }

  override def draw() {
    if(mousePressed) cellAt(mouseVec).nextVec2 += mouseVec - Vec2(pmouseX, pmouseY)
    background(0)
    for(c <- vectorField.flatten) {
      c.step()
    }
    for(c <- vectorField.flatten) {
      c.updateAndDraw()
    }
    loadPixels()
    for(b <- balls) {
      b.step()
//      b.draw()
//      println(b.pos)
      pixels(b.pos.y.toInt*width + b.pos.x.toInt) = lerpColor(pixels(b.pos.y.toInt*width + b.pos.x.toInt), color(255), .5f)//color(255)
    }
    updatePixels()
    println(frameRate)

//    println(profileReport)
    resetProfiler()
  }

  override def keyPressed() {
  }
}