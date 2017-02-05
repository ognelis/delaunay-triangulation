package delone
import java.util.concurrent.atomic.AtomicInteger

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scalafx.scene.canvas.Canvas
import scalafx.scene.paint.Color

/**
  * Created by Admin on 28.11.2016.
  */
class Triangle(var a: Vertex, var b: Vertex, var c:Vertex) extends Drawable {
  require(a < b)
  require(b < c)

  val id = Triangle.seq.getAndIncrement()
  var isDrawable: Boolean = true

  override def equals(obj: scala.Any): Boolean = obj match {
    case that: Triangle =>
      this.a == that.a &&
        this.b == that.b &&
        this.c == that.c
    case _ => false
  }


  val edges: Array[Edge] = {
    val first  = Edge(b, c)
    val second = Edge(a, c)
    val third  = Edge(a, b)
    Array(first, second, third)
  }

  def updateEdges():Unit = {
    edges.update(0, Edge(b, c))
    edges.update(1, Edge(a, c))
    edges.update(2, Edge(a, b))
  }

  def getEdgeVertxesEqality(that: Triangle) = {
    val edge = this.edges intersect that.edges
    if (edge.length == 1) Some(edge.head) else None
  }


  def neigbourTriangles = {
    Array(edges(0).triangle2, edges(1).triangle2, edges(2).triangle2).filter(_ != null)
  }


  def edgeWithOtherTriangle(thatTriangle: Triangle): Unit = {
    this.edges foreach (thisEdge =>
      thatTriangle.edges foreach { thatEdge =>
        if (thisEdge == thatEdge) {
          thisEdge.triangle2 = thatTriangle
          thatEdge.triangle2 = this
        }
      })
  }

  def vertexes = ArrayBuffer(a, b ,c)

  def calculateAngle(a: Vertex, b: Vertex, c: Vertex)(canvas: Canvas) = {
    val ab = scala.math.sqrt((b.x - a.x)*(b.x - a.x) + (b.y - a.y)*(b.y - a.y))
    val bc = scala.math.sqrt((c.x - b.x)*(c.x - b.x) + (c.y - b.y)*(c.y - b.y))
    val ac = scala.math.sqrt((a.x - c.x)*(a.x - c.x) + (a.y - c.y)*(a.y - c.y))
    val cosA = (ab*ab + ac*ac - bc*bc)/(2.0*ab*ac)

    val res = scala.math.acos(cosA) * (180.0 / scala.math.Pi)
    if (res.isNaN) {
      println(a, b, c)
      draw(canvas)

      throw new Exception("angle is zero")
    } else res
  }


//  def checkAngle(canvas: Canvas): Unit = {
//    neigbourTriangles foreach{neighbour =>
//      val edge = getEdgeWithMyNeigbour(neighbour).get
//      val thisVertex = (vertexes - edge.a - edge.b).head
//      val thatVertex = (neighbour.vertexes - edge.a - edge.b).head
//      require((vertexes - edge.a - edge.b).length == 1)
//      require((neighbour.vertexes - edge.a - edge.b).length == 1)
//      require((calculateAngle(thisVertex, edge.a, edge.b) + calculateAngle(thatVertex, edge.a, edge.b)) < 180.0, s"$this $neighbour")
//    }
//  }

//  def angleProtiv(thisTriangle: Triangle, thatTriangle: Triangle, canvas: Canvas): Unit  = {
//    val edgeOpt = thisTriangle.getEdgeVertxesEqality(thatTriangle)
//    if (edgeOpt.isDefined) {
//      val edge = edgeOpt.get
//      val thisVertex = (thisTriangle.vertexes - edge.a - edge.b).head
//      val thatVertex = (thatTriangle.vertexes - edge.a - edge.b).head
//
//      require((thisTriangle.vertexes - edge.a - edge.b).length == 1)
//      require((thatTriangle.vertexes - edge.a - edge.b).length == 1)
//
////      val points1 = Array(thisVertex, edge.a, edge.b).sorted
////      val points2 = Array(thatVertex, edge.a, edge.b).sorted
////      val (c, d) = if (thisVertex > thatVertex) (thatVertex, thisVertex) else (thisVertex, thatVertex)
////      if (!checkDelaunayCondition(c, edge.a, d, edge.b)) {
//
//      if ((calculateAngle(thisVertex, edge.a, edge.b)(canvas) + calculateAngle(thatVertex, edge.a, edge.b)(canvas)) >= 180.0) {
//        val newVertexesForThis = Array(edge.a, thisVertex, thatVertex).sorted
//        val newVertexesForThat = Array(edge.b, thisVertex, thatVertex).sorted
//
//        val neighbours = (thisTriangle.neigbourTriangles ++ thatTriangle.neigbourTriangles).toBuffer -- Array(thisTriangle, thatTriangle)
//
//        thisTriangle.a = newVertexesForThis(0)
//        thisTriangle.b = newVertexesForThis(1)
//        thisTriangle.c = newVertexesForThis(2)
//        thisTriangle.updateEdges()
//
//
//        thatTriangle.a = newVertexesForThat(0)
//        thatTriangle.b = newVertexesForThat(1)
//        thatTriangle.c = newVertexesForThat(2)
//        thatTriangle.updateEdges()
//
//        thisTriangle.edgeWithOtherTriangle(thatTriangle)
//
//        neighbours foreach { x =>
//          thisTriangle.edgeWithOtherTriangle(x)
//          thatTriangle.edgeWithOtherTriangle(x)
//        }
//        neighbours foreach { x => if (thisTriangle.getEdgeVertxesEqality(x).isDefined) thisTriangle.angleProtiv(thisTriangle, x,canvas) else thatTriangle.angleProtiv(thisTriangle, x,canvas)}
//      }
//    }
//  }




  def checkDelaunayCondition(vertex0: Vertex, vertex1: Vertex, vertex2: Vertex, vertex3: Vertex): Boolean = {
    val cosA = (vertex0.x - vertex1.x)*(vertex0.x - vertex3.x) + (vertex0.y - vertex1.y)*(vertex0.y - vertex3.y)
    val cosB = (vertex2.x - vertex1.x)*(vertex2.x - vertex3.x) + (vertex2.y - vertex1.y)*(vertex2.y - vertex3.y)
    (cosA, cosB) match {
      case (_, _) if cosA >= 0.0 && cosB >= 0.0 => true
      case (_, _) if cosA < 0.0  && cosB < 0.0  => false
      case _ =>
        (((vertex0.x - vertex1.x)*(vertex0.y - vertex3.y) - (vertex0.x - vertex3.x)*(vertex0.y - vertex1.y))* cosB +
          cosA * ((vertex2.x - vertex1.x) * (vertex2.y - vertex3.y) - (vertex2.x - vertex3.x) * (vertex2.y - vertex1.y))) >= 0.0
    }
  }

  def crossProduct(a:Vertex, b:Vertex, c:Vertex): Double = (b.x - a.x)*(c.y - a.y) - (b.y - a.y)*(c.x - a.x)

  @tailrec
  final def findTrinagleForThatPoint(vertex: Vertex): Triangle = {
    val first  = edges(0)
    val second = edges(1)
    val third  = edges(2)

    if ((crossProduct(a, first.a, first.b) >= 0.0 ^ crossProduct(vertex, first.a, first.b) >= 0.0) && first.triangle2 != null) first.triangle2.findTrinagleForThatPoint(vertex)
    else if ((crossProduct(b, second.a, second.b) >= 0.0 ^ crossProduct(vertex, second.a, second.b) >= 0.0) && second.triangle2 != null) second.triangle2.findTrinagleForThatPoint(vertex)
    else if ((crossProduct(c, third.a, third.b)  >= 0.0 ^ crossProduct(vertex, third.a, third.b) >= 0.0) && third.triangle2 != null) third.triangle2.findTrinagleForThatPoint(vertex)
    else this
  }

  override def draw(canvas: Canvas): Unit = {
    val height = 800.0
//    canvas.graphicsContext2D.strokeText(a.toString, a.x,a.y)
//    canvas.graphicsContext2D.strokeText(b.toString, b.x,b.y)
//    canvas.graphicsContext2D.strokeText(c.toString, c.x,c.y)
    canvas.graphicsContext2D.strokeLine(a.x, height - a.y, b.x, height - b.y)
    canvas.graphicsContext2D.strokeLine(b.x, height - b.y, c.x, height - c.y)
    canvas.graphicsContext2D.strokeLine(c.x, height - c.y, a.x, height - a.y)
  }

  override def toString: String = s"my id is: $id; $a $b $c"

  def center = Vertex((a.x + b.x + c.x)/3, (a.y + b.y + c.y)/3)
  def drawCenter(canvas: Canvas) = {
    val cent = center
    canvas.graphicsContext2D.strokeText(id.toString,cent.x, cent.y)
  }
}

object Triangle {
  def apply(a: Vertex, b: Vertex, c: Vertex): Triangle = {
    val points = Array(a, b, c).sorted
    new Triangle(points(0), points(1), points(2))
  }

  private val seq = new AtomicInteger(0)
}
