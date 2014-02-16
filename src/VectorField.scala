import org.zhang.geom.Vec2
import processing.core._
import org.zhang.lib.MyPApplet
import org.zhang.lib._
import SimpleOpenNI._

class VectorField extends MyPApplet {

  object SkeletonTracker {
    val context = new SimpleOpenNI(VectorField.this)
    if(!context.isInit)
    {
       println("Can't init SimpleOpenNI, maybe the camera is not connected!")
       exit()
    }
    context.enableDepth()
    context.enableUser()

    def update() = {
      val oldUsers = context.getUsers.filter(context.isTrackingSkeleton(_))
      val oldSkels = oldUsers.map(skeletalPoints _)
      context.update()
      val skeletons = for((userId, idx) <- oldUsers.zipWithIndex if context.isTrackingSkeleton(userId))
        yield (oldSkels(idx), skeletalPoints(userId))
      skeletons
    }

    import SimpleOpenNIConstants._
    lazy val SKELETAL_EDGES = List(
      (SKEL_LEFT_SHOULDER, SKEL_LEFT_ELBOW),
      (SKEL_LEFT_ELBOW, SKEL_LEFT_HAND),
      (SKEL_NECK, SKEL_LEFT_SHOULDER),
      (SKEL_LEFT_SHOULDER, SKEL_TORSO),
      (SKEL_TORSO, SKEL_LEFT_HIP),
      (SKEL_LEFT_HIP, SKEL_LEFT_KNEE),
      (SKEL_LEFT_KNEE, SKEL_LEFT_FOOT),

      (SKEL_RIGHT_SHOULDER, SKEL_RIGHT_ELBOW),
      (SKEL_RIGHT_ELBOW, SKEL_RIGHT_HAND),
      (SKEL_NECK, SKEL_RIGHT_SHOULDER),
      (SKEL_RIGHT_SHOULDER, SKEL_TORSO),
      (SKEL_TORSO, SKEL_RIGHT_HIP),
      (SKEL_RIGHT_HIP, SKEL_RIGHT_KNEE),
      (SKEL_RIGHT_KNEE, SKEL_RIGHT_FOOT),

      (SKEL_NECK, SKEL_HEAD)
    )

    type Skeleton = Map[Int, Vec2]

    def skeletalPoints(userId: Int): Skeleton = List(SKEL_HEAD,
      SKEL_LEFT_ELBOW,
      SKEL_LEFT_FINGERTIP,
      SKEL_LEFT_FOOT,
      SKEL_LEFT_HAND,
      SKEL_LEFT_HIP,
      SKEL_LEFT_KNEE,
      SKEL_LEFT_SHOULDER,
      SKEL_NECK,
      SKEL_RIGHT_ELBOW,
      SKEL_RIGHT_FINGERTIP,
      SKEL_RIGHT_FOOT,
      SKEL_RIGHT_HAND,
      SKEL_RIGHT_HIP,
      SKEL_RIGHT_KNEE,
      SKEL_RIGHT_SHOULDER,
      SKEL_TORSO).map { p =>
        val v = new PVector()
        context.getJointPositionSkeleton(userId, p, v)
        val proj = new PVector()
        context.convertRealWorldToProjective(v, proj) // each point is in (0,0) -> (640, 480)
        (p, Vec2(map(proj.x, 0, 640, 0, width), map(proj.y, 0, 480, 0, height)))
      }.toMap
  }

  def onNewUser(curContext: SimpleOpenNI , userId: Int) {
    println("onNewUser - userId: " + userId);
    println("\tstart tracking skeleton");

    curContext.startTrackingSkeleton(userId);
  }

  def onLostUser(curContext: SimpleOpenNI , userId: Int) {
    println("onLostUser - userId: " + userId);
  }

  def onVisibleUser(curContext: SimpleOpenNI , userId:Int) {
    //println("onVisibleUser - userId: " + userId);
  }

  val NUM_CELLS = 120

  import PApplet._
  import PConstants._

  class Cell(val x: Int, val y: Int, var vec2:PVector = new PVector()) {
    val pos = new PVector(map(x+.5f, 0, NUM_CELLS, 0, width), map(y+.5f, 0, NUM_CELLS, 0, height))

    lazy val neighbors = for(v <- 0 until 4) yield vectorField(wrapIndex(nxList(v) + x))(wrapIndex(nyList(v) + y))

    var balls = collection.mutable.ArrayBuffer[Ball]()
    var nextVec2 = new PVector()

    def step() {
      //seed
      val theta = random(TWO_PI)
      val radius = random(1)
      nextVec2.add(radius*cos(theta), radius*sin(theta), 0)
//      nextVec2 += Vec2(noise(pos.x / 300, pos.y / 300, millis() / 3000f) - .5f, noise(pos.x / 300 + 12, pos.y / 300 + 7, millis() / 3000f) - .5f) * .1f

      // keep some for yourself
      nextVec2.add(vec2.x * stasis, vec2.y * stasis, 0)

      for(i <- 0 until 4) {
        val n = neighbors(i)
        val nx = nxList(i)
        val ny = nyList(i)
        // give equal diffusion to neighbors
        n.nextVec2.add( vec2.x * diffuEqScalar, vec2.y * diffuEqScalar, 0 )

        val dot = vec2.dot(nx, ny, 0)
        val proportion = if (dot == 0) 0 else max(dot * abs(dot) / vec2.magSq(), 0)
        val diffuDirScalarDotted = diffuDirScalar * proportion
        n.nextVec2.add(vec2.x * diffuDirScalarDotted, vec2.y * diffuDirScalarDotted, 0 )
      }

      //dissipation
      nextVec2.mult(dissipation)

      //bias slow moving fields towards the bottom, adding a bit of gravity
      if(abs(nextVec2.y) < 3f) nextVec2.y += .01f
    }

    def updateAndDraw() {
      vec2.set(nextVec2)
//      vec2.mult(.8f)
//      vec2.add(nextVec2.x * .2f, nextVec2.y * .2f, 0)
      if(java.lang.Float.isNaN(vec2.x)) vec2.set(0, 0, 0);
      nextVec2.set(0, 0, 0)
      balls.clear()

      val c = vec2.mag * 10 match {
        case e if e < 160 => color(e)
        case e if e < 600 => lerpColor(color(160), color(255, 190, 160), (e - 160) / (600 - 160))
        case e if e < 1200 => lerpColor(color(255, 190, 160), color(210, 40, 10), (e - 600) / (1200 - 600))
        case e => color(210, 40, 10)
      }
      stroke(c)
      vertex(pos.x, pos.y)
      vertex(pos.x + vec2.x, pos.y + vec2.y)
//      lineShape.setVertex(idx*2 + 1, pos.x + vec2.x, pos.y + vec2.y)
//      shape(lineShape)
//      println("drew " + x+", "+y)
//      line(pos.x, pos.y, pos.x + vec2.x, pos.y + vec2.y)
//      line(pos, pos + vec2 * 20)
//      fill(c); noStroke()
//      rect(pos - CELL_SIZE / 2, CELL_SIZE.x, CELL_SIZE.y)
    }
  }

  var randomState = System.nanoTime()
  def randomLong = {
    randomState ^= (randomState << 21)
    randomState ^= (randomState >>> 35)
    randomState ^= (randomState << 4)
    randomState
  }

  //fast random in (-1..1)
  def frandom = (randomLong % 4294967296L) / 4294967296.0f


  class Ball(var pos: PVector, var vel: PVector = new PVector()) {
    def step() {
      val cell = cellAt(pos)
//      pos.add(frandom * cell.balls.length / 5000f, frandom * cell.balls.length / 5000f, 0)
//      if(keyPressed) {
        val exp = pow(cell.balls.length, 1.5f) / 100
        pos.add(frandom * exp, frandom * exp, 0)
//      }
      if(java.lang.Float.isNaN(vel.x)) vel.set(0, 0, 0)
      vel.mult(.5f)
      vel.add(cell.vec2.x * .5f, cell.vec2.y * .5f, 0)
      cell.balls += this
      pos.set(wrapWorldX(pos.x + vel.x), wrapWorldY(pos.y + vel.y), 0)
      if(java.lang.Float.isNaN(pos.x)) pos.set(0, 0, 0)

      vel.mult(.95f)
    }
  }

  def wrapIndex(x:Int) = ((x % NUM_CELLS) + NUM_CELLS) % NUM_CELLS
  def wrapWorldX(x:Float) = ((x % width) + width) % width
  def wrapWorldY(x:Float) = ((x % height) + height) % height

  //world position into indexed position
  def cellAt(pos: PVector) = vectorField(wrapIndex(map(pos.x, 0, width, 0, NUM_CELLS).toInt))(wrapIndex(map(pos.y, 0, height, 0, NUM_CELLS).toInt))
  def indexPosition(pos: Vec2) = Vec2(constrain(map(pos.x, 0, width, 0, NUM_CELLS).toInt, 0, NUM_CELLS), constrain(map(pos.y, 0, height, 0, NUM_CELLS).toInt, 0, NUM_CELLS))

  //from, to are index locations
  def pushLine(from:Vec2, to:Vec2, force:Vec2) {
    val dx = abs(to.x-from.x)
    val dy = abs(to.y-from.y)
    val sx = if(from.x < to.x) 1 else -1
    val sy = if(from.y < to.y) 1 else -1
    var err = dx - dy
    var Vec2(x0, y0) = from

    while(!(x0 == to.x && y0 == to.y)) {
      vectorField(x0.toInt)(y0.toInt).nextVec2.add(force.x, force.y, 0)
      if(2*err > -dy) {
        err -= dy
        x0 += sx
      }
      if(x0 == to.x && y0 == to.y) {
        vectorField(x0.toInt)(y0.toInt).nextVec2.add(force.x, force.y, 0)
      } else {
        if(2*err < dx) {
          err += dx
          y0 += sy
        }
      }
    }
  }

  def pushTri(v0: Vec2, v1: Vec2, v2: Vec2, f0: Vec2, f1: Vec2, f2: Vec2) {
    // Compute triangle bounding box
    var minX = min(v0.x, v1.x, v2.x).toInt
    var minY = min(v0.y, v1.y, v2.y).toInt
    var maxX = max(v0.x, v1.x, v2.x).toInt
    var maxY = max(v0.y, v1.y, v2.y).toInt

    // Clip against screen bounds
    minX = max(minX, 0)
    minY = max(minY, 0)
    maxX = min(maxX, NUM_CELLS - 1)
    maxY = min(maxY, NUM_CELLS - 1)
//
//    // Rasterize
    val vs1 = v1 - v0
    val vs2 = v2 - v0
    for (y <- minY to maxY;
         x <- minX to maxX) {
      val q = Vec2(x - v0.x, y - v0.y)

      val s = (vs1 cross q) / (vs1 cross vs2)
      val t = (q cross vs2) / (vs1 cross vs2)

      if ( (t >= 0) && (s >= 0) && (t + s <= 1))
      { /* inside triangle */
        val force = f0*s + f1*t + f2*(1 - t - s)
        vectorField(x)(y).nextVec2.add(force.x, force.y, 0)
      }
    }
  }

  val dissipation = .99f
  val stasis = 0.1f
  val directionalBias = 0.9f


  val diffuEqScalar = ( (1 - stasis) * (1 - directionalBias) ) / 4
  val diffuDirScalar = (1 - stasis) * directionalBias
  val nxList = Seq(-1, 1, 0, 0)
  val nyList = Seq(0, 0, -1, 1)

  lazy val vectorField = collection.mutable.Seq.tabulate(NUM_CELLS, NUM_CELLS)((x, y) => new Cell(x, y))
  lazy val balls = collection.mutable.Seq.fill(50000){ new Ball(new PVector(random(width), random(height))) }

  override def setup() {
    size(displayWidth, displayHeight)
    noSmooth()
    SkeletonTracker
  }

  override def draw() {
    if(mousePressed) {
//      cellAt(mouseVec).nextVec2 += mouseVec - Vec2(pmouseX, pmouseY)
//      pushLine(indexPosition(Vec2(pmouseX, pmouseY)), indexPosition(mouseVec), mouseVec - Vec2(pmouseX, pmouseY))
      val force = mouseVec - Vec2(pmouseX, pmouseY)
      pushTri(indexPosition(Vec2(pmouseX, pmouseY)), indexPosition(mouseVec), indexPosition(Vec2(width/2, height/2)), force, force, Vec2())
    }

    background(0)
//    image(SkeletonTracker.context.userImage(), 0, 0)
    for(skel <- SkeletonTracker.update) {
      for((fromId, toId) <- SkeletonTracker.SKELETAL_EDGES) {
        val from1 = skel._1(fromId)
        val from2 = skel._2(fromId)
        val dFrom = (from2 - from1) / 4
        val to1 = skel._1(toId)
        val to2 = skel._2(toId)
        val dTo = (to2 - to1) / 4

        stroke(255, 255, 0, 64)
        line(from1, to1)
        line(from1, from2)
        line(from2, to2)
        line(to1, to2)
        pushTri(indexPosition(from1), indexPosition(from2), indexPosition(to1), dFrom, dFrom, dTo)
        pushTri(indexPosition(to1), indexPosition(to2), indexPosition(from2), dTo, dTo, dFrom)
      }
    }

    for(c <- vectorField.flatten) {
      c.step()
    }
    beginShape(LINES)
    for(c <- vectorField.flatten) {
      c.updateAndDraw()
    }
    endShape()
    loadPixels()
    for(b <- balls) {
      b.step()
      pixels(b.pos.y.toInt*width + b.pos.x.toInt) = lerpColor(pixels(b.pos.y.toInt*width + b.pos.x.toInt), 0xff00ffff, .5f)//color(255)
    }
    updatePixels()
    println(frameRate)
  }

  override def keyPressed() {
  }
}