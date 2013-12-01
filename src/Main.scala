import org.zhang.geom.Vec2
import processing.core._
import org.zhang.lib.MyPApplet
import org.zhang.lib._

class Main extends MyPApplet {

  import PApplet._;
  import PConstants._;

  val NUM_CELLS = 70
  lazy val dimensions = Vec2(width, height)
  lazy val CELL_SIZE = dimensions / NUM_CELLS

  override def setup() {
//    size(displayWidth, displayHeight, P2D)
    size(600, 600, P2D)
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

    lazy val neighbors = for (v <- List(Vec2(-1, 0), Vec2(1, 0), Vec2(0, -1), Vec2(0, 1)).map {_ + Vec2(x, y)})
                           yield vectorField(wrapIndex(v.x.toInt))(wrapIndex(v.y.toInt))

    var balls = collection.mutable.ArrayBuffer[Ball]()
    var nextVec2 = Vec2()

    def offsetTo(c: Cell) = {
      import zhang.Methods.distance
      Vec2(distance(x, c.x, NUM_CELLS), distance(y, c.y, NUM_CELLS))
    }

    def step() {
      //seed
//      nextVec2 += Vec2(noise(pos.x / 300, pos.y / 300, millis() / 3000f) - .5f, noise(pos.y / 270, pos.x / 420 + 14, millis() / 3000f) - .5f)

      val stasis = 0.2f
      // keep some for yourself
      nextVec2 += vec2 * stasis

      val directionalBias = .5f
      for(n <- neighbors) {
        // give equal diffusion to neighbors
        n.nextVec2 += (vec2 * (1 - stasis) * (1 - directionalBias) ) / 4

        val dot = vec2.dot(offsetTo(n))
        val proportion = if (dot == 0) 0 else max(dot * abs(dot) / vec2.mag2, 0)
        n.nextVec2 += (vec2 * (1 - stasis) * directionalBias ) * proportion
      }

      //dissipation
//      nextVec2 *= .95f
    }

    def updateAndDraw() {
      val offset = nextVec2 - vec2
      vec2 = nextVec2
      nextVec2 = Vec2()
      balls.clear()

      val c = vec2.mag * 10 match {
        case e if e < 160 => color(e)
        case e if e < 600 => lerpColor(color(160), color(255, 190, 160), (e - 160) / (600 - 160))
        case e if e < 1200 => lerpColor(color(255, 190, 160), color(210, 40, 10), (e - 600) / (1200 - 600))
        case e => color(210, 40, 10)
      }
      stroke(c)
      line(pos, pos + vec2)
//      line(pos, pos + vec2 * 20)
//      fill(c); noStroke()
//      rect(pos - CELL_SIZE / 2, CELL_SIZE.x, CELL_SIZE.y)
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
  def indexPosition(pos: Vec2) = Vec2(wrapIndex(map(pos.x, 0, width, 0, NUM_CELLS).toInt), wrapIndex(map(pos.y, 0, height, 0, NUM_CELLS).toInt))

  lazy val vectorField = collection.mutable.Seq.tabulate(NUM_CELLS, NUM_CELLS)((x, y) => new Cell(x, y))
  lazy val balls = collection.mutable.Seq.fill(20000){ new Ball(Vec2(random(width), random(height))) }

  //from, to are index locations
  def pushField(from:Vec2, to:Vec2, force:Vec2) {
    val dx = abs(to.x-from.x)
    val dy = abs(to.y-from.y)
    val sx = if(from.x < to.x) 1 else -1
    val sy = if(from.y < to.y) 1 else -1
    var err = dx - dy
    var Vec2(x0, y0) = from

    while(!(x0 == to.x && y0 == to.y)) {
      vectorField(x0.toInt)(y0.toInt).nextVec2 += force
      if(2*err > -dy) {
        err -= dy
        x0 += sx
      }
      if(x0 == to.x && y0 == to.y) {
        vectorField(x0.toInt)(y0.toInt).nextVec2 += force
      } else {
        if(2*err < dx) {
          err += dx
          y0 += sy
        }
      }
    }
  }

  override def draw() {
    if(mousePressed) {
//      cellAt(mouseVec).nextVec2 += mouseVec - Vec2(pmouseX, pmouseY)
      pushField(indexPosition(Vec2(pmouseX, pmouseY)), indexPosition(mouseVec), mouseVec - Vec2(pmouseX, pmouseY))
    }
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