package geometry

import java.lang.Double


case class Vertex(x: Double, y: Double) extends Ordered[Vertex] {

  override def compare(that: Vertex): Int = {
    val xComparison = Double.compare(x, that.x)
    if (xComparison > 0) 1
    else {
      val yComparison = Double.compare(y, that.y)
      if (xComparison == 0 && yComparison == 0) 0
      else if (xComparison == 0 && yComparison > 0) 1
      else -1
    }
  }
}

