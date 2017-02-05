package delone

/**
  * Created by Admin on 28.11.2016.
  */
case class Vertex(x: Double, y: Double) extends Ordered[Vertex] {
  def ~=(x: Double, y: Double, precision: Double = 0.00000001) = {
    if ((x - y).abs < precision) true else false
  }

  def compare(that: Vertex): Int = {
    if (x > that.x) 1
    else if (~=(x, that.x) && ~=(y,that.y)) 0
    else if (~=(x, that.x) && (y > that.y)) 1 else -1
  }
}

