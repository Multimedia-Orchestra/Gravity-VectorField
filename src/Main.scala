import org.zhang.geom.Vec2
import processing.core._
import org.zhang.lib.MyPApplet
import org.zhang.lib._

class Main extends MyPApplet {

  import PApplet._;
  import PConstants._;

  val NUM_CELLS = 100

  override def setup() {
    size(800, 600, P2D)
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

  lazy val vectorField = collection.mutable.Seq.tabulate(NUM_CELLS, NUM_CELLS)((x, y) => new Cell(x, y))

  class Cell(val x: Int, val y: Int, var vec2:Vec2 = Vec2()) {
    val pos = Vec2(map(x+.5f, 0, NUM_CELLS, 0, width), map(y+.5f, 0, NUM_CELLS, 0, height))

    var nextVec2 = vec2
    def draw() {
      stroke(vec2.mag)
//      line(pos, pos + vec2.normalize * pow(vec2.mag, 1 / 4f))
      line(pos, pos + vec2)
    }
    def step() {
      //seed
      nextVec2 += Vec2(noise(pos.x / 300, pos.y / 300, millis() / 3000f) - .5f, noise(pos.y / 270, pos.x / 420 + 14, millis() / 3000f) - .5f)

      def wrap(x:Int) = (x + NUM_CELLS) % NUM_CELLS
      val neighbors =
        for(v <- List(Vec2(-1, 0), Vec2(1, 0), Vec2(0, -1), Vec2(0, 1)).map{_ + Vec2(x, y)})
        yield vectorField(wrap(v.x.toInt))(wrap(v.y.toInt))

      //equal diffusion
//      val diffusion = neighbors.map{_.vec2}.reduce(_ + _) / 4

      //directed diffusion
      val diffusion = neighbors.map{ c =>
        val direction = (pos - c.pos).normalize
        val mag = direction dot c.vec2
        val proportion = max(0, if(c.vec2.mag == 0) 0 else mag / c.vec2.mag)
        val contribution = c.vec2 * proportion
        contribution
      }.reduce(_ + _)
//      println(diffusion)

      val stasis = 1f
      nextVec2 = nextVec2 * stasis + diffusion * (1 - stasis)

//      if(nextVec2.mag > 50) nextVec2 = Vec2()


    }
    def update() {
      vec2 = nextVec2
    }
  }

  override def draw() {
    if(mousePressed) vectorField(map(mouseX, 0, width, 0, NUM_CELLS).toInt)(map(mouseY, 0, height, 0, NUM_CELLS).toInt).nextVec2 += Vec2(10)
    background(0)
    for(c <- vectorField.flatten) {
      c.step()
    }
    for(c <- vectorField.flatten) {
      c.update()
      c.draw()
    }
    println(frameRate)

//    println(profileReport)
    resetProfiler()
  }

  override def keyPressed() {
  }
}