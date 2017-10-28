package delone

import convexhull.ConvexHull

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scalafx.scene.canvas.Canvas

/**
  * Created by Admin on 23.01.2017.
  */
class Delone(vertexes: ArrayBuffer[Vertex]) {
  @tailrec
  private def firstTriangles(initialPoint: Vertex, points: List[Vertex], acc: List[Triangle]): List[Triangle] = points match {
    case head::next::Nil => Triangle(initialPoint, head, next) :: acc
    case head::next::tail => firstTriangles(initialPoint, next::tail, Triangle(initialPoint, head, next) :: acc)
    case _ => acc
  }

  val (pointsConvex, pointsNotInConvex) =
    ConvexHull.buildConvexHull(vertexes)


  val triangles = {
    val trianglesInCovexHull = firstTriangles(pointsConvex.head, pointsConvex.tail, Nil).toBuffer
    val trianglesAll = new ArrayBuffer[Triangle](trianglesInCovexHull.length + pointsNotInConvex.length*2)
    trianglesAll ++= trianglesInCovexHull
  }

  def makeEdges() = triangles.combinations(2).foreach(x => x.head.edgeWithOtherTriangle(x(1)))

  def checkDelaunayCondition(vertex0: Vertex, vertex1: Vertex, vertex2: Vertex, vertex3: Vertex): Boolean = {
    val cosA = (vertex0.x - vertex1.x)*(vertex0.x - vertex3.x) + (vertex0.y - vertex1.y)*(vertex0.y - vertex3.y)
    val cosB = (vertex2.x - vertex1.x)*(vertex2.x - vertex3.x) + (vertex2.y - vertex1.y)*(vertex2.y - vertex3.y)

    (cosA, cosB) match {
      case (_, _) if cosA >= 0.0 && cosB >= 0.0 => true
      case (_, _) if cosA < 0.0  && cosB < 0.0  => false
      case _ =>
        val result = ((vertex0.x - vertex1.x)*(vertex0.y - vertex3.y) - (vertex0.x - vertex3.x)*(vertex0.y - vertex1.y))* cosB +
          cosA * ((vertex2.x - vertex3.x) * (vertex2.y - vertex1.y) - (vertex2.x - vertex1.x) * (vertex2.y - vertex3.y))
        result >= 0.0
    }
  }

  def angleProtiv(thisTriangle: Triangle, thatTriangle: Triangle): Unit  = {
    val edgeOpt = thisTriangle.getEdgeVertxesEqality(thatTriangle)
    if (edgeOpt.isDefined) {
      val edge = edgeOpt.get
      val thisVertex = (thisTriangle.vertexes - edge.a - edge.b).head
      val thatVertex = (thatTriangle.vertexes - edge.a - edge.b).head

      def ccw(a:Vertex, b:Vertex, c:Vertex): Double = (b.x - a.x)*(c.y - a.y) - (b.y - a.y)*(c.x - a.x)
      val (c, d) = if (ccw (thisVertex, edge.a, edge.b) < 0.0) (thatVertex, thisVertex) else (thisVertex, thatVertex)

      if (!checkDelaunayCondition(c, edge.a, d, edge.b)) {
        val newVertexesForThis = Array(edge.a, thisVertex, thatVertex).sorted
        val newVertexesForThat = Array(edge.b, thisVertex, thatVertex).sorted

        val neighbours = (thisTriangle.neigbourTriangles ++ thatTriangle.neigbourTriangles) diff Seq(thisTriangle, thatTriangle)

        thisTriangle.a = newVertexesForThis(0)
        thisTriangle.b = newVertexesForThis(1)
        thisTriangle.c = newVertexesForThis(2)
        thisTriangle.updateEdges()


        thatTriangle.a = newVertexesForThat(0)
        thatTriangle.b = newVertexesForThat(1)
        thatTriangle.c = newVertexesForThat(2)
        thatTriangle.updateEdges()

        thisTriangle.edgeWithOtherTriangle(thatTriangle)


        neighbours foreach { x =>
          thisTriangle.edgeWithOtherTriangle(x)
          thatTriangle.edgeWithOtherTriangle(x)
        }
        neighbours foreach { x => if (thisTriangle.getEdgeVertxesEqality(x).isDefined) angleProtiv(x, thisTriangle) else angleProtiv(x, thatTriangle)}
      }
    }
  }


  def addVertexAndRebuild(vertex: Vertex): Unit = {
    val triangleToRebuild = triangles.last.findTrinagleForThatPoint(vertex)
    def neigbours = triangleToRebuild.neigbourTriangles


    val firstTriangle  = Triangle(vertex, triangleToRebuild.b, triangleToRebuild.c)
    val secondTriangle = Triangle(triangleToRebuild.a, vertex, triangleToRebuild.c)
    val thirdTriangle  = Triangle(triangleToRebuild.a, triangleToRebuild.b, vertex)

    val trianglesToAdd = Array(firstTriangle, secondTriangle, thirdTriangle)

    triangles ++= trianglesToAdd
    triangleToRebuild.isDrawable = false

    trianglesToAdd.combinations(2).foreach(x => x(0).edgeWithOtherTriangle(x(1)))
    neigbours.foreach(x => trianglesToAdd.foreach(y => x.edgeWithOtherTriangle(y)))

    (trianglesToAdd ++ neigbours).combinations(2).foreach(x => angleProtiv(x(0),x(1)))
  }

  def deloneAll(): Unit = pointsNotInConvex.tail.foreach(vertex => addVertexAndRebuild(vertex))
}

object Delone {
  def calculateAngle(a: Vertex, b: Vertex, c: Vertex) = {
    val ab = scala.math.sqrt((b.x - a.x)*(b.x - a.x) + (b.y - a.y)*(b.y - a.y))
    val bc = scala.math.sqrt((c.x - b.x)*(c.x - b.x) + (c.y - b.y)*(c.y - b.y))
    val ac = scala.math.sqrt((a.x - c.x)*(a.x - c.x) + (a.y - c.y)*(a.y - c.y))
    val cosA = (ab*ab + ac*ac - bc*bc)/(2.0*ab*ac)

    val res = scala.math.acos(cosA) * (180.0 / scala.math.Pi)
    if (res.isNaN) {
      println(a, b, c)
      throw new Exception("angle is zero")
    } else res
  }
}
