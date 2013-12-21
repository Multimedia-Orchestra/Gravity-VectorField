import processing.core._
import org.zhang.lib._

class ReactionDiffusion extends MyPApplet {

  import PApplet._;
  import PConstants._;

  override def setup() {
    size(500, 500, P2D)
    u = Array.tabulate(width, height){(a, b) => random(1)}
    v = Array.tabulate(width, height){(a, b) => 0}
    du = Array.tabulate(width, height){(a, b) => 0}
    dv = Array.tabulate(width, height){(a, b) => 0}
  }

  var u, v, du, dv: Array[Array[Float]] = _

  var f = .037f
  var k = .06f
  var diffU = .16f
  var diffV = .08f


  def mod(x: Int, w: Int = width) = (x + w) % w

  override def draw() {
    loadPixels()
    (0 until width*height).par.foreach{i =>
      val x = i % width
      val y = i / width

      val currU = u(x)(y)
      val currV = v(x)(y)
      val uvv = currU * currV * currV
      
      val lapU = u(mod(x+1))(y) + u(mod(x-1))(y) + u(x)(mod(y+1, height)) + u(x)(mod(y-1, height)) - 4*currU
      val lapV = v(mod(x+1))(y) + v(mod(x-1))(y) + v(x)(mod(y+1, height)) + v(x)(mod(y-1, height)) - 4*currV

      du(x)(y) = diffU * lapU - uvv + f*(1 - currU)
      dv(x)(y) = diffV * lapV + uvv - (k + f)*currV

    }
    (0 until width*height).par.foreach{i =>
      val x = i % width
      val y = i / width
      u(x)(y) += du(x)(y)
      v(x)(y) += dv(x)(y)

      if(mousePressed && dist(x, y, mouseX, mouseY) < 20) {
        v(x)(y) = 1
      }
      pixels(i) = color(0, v(x)(y) * 255, du(x)(y) * 255)
    }
    updatePixels()
    println(frameRate)
  }
}