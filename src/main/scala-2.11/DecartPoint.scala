/**
  * Created by Admin on 20.09.2016.
  */
case class DecartPoint(x: Double, y: Double) extends Ordered[DecartPoint] {
  def ~=(x: Double, y: Double, precision: Double) = {
    if ((x - y).abs < precision) true else false
  }

  def compare(that: DecartPoint): Int = {
    if (x > that.x) 1
    else if (~=(x,that.x,0.00001) && ~=(y,that.y,0.00001)) 0
    else if (~=(x,that.x,0.00001) && y < that.y) 1  else -1
  }
}