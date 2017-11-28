package delone

import convexhull.ConvexHull
import delone.cache.DynamicCache
import geometry.{Triangle, Vertex}

import scala.annotation.tailrec
import scala.collection.mutable
import scalafx.scene.canvas.Canvas


object Delone {
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
}


class Delone(vertexes: mutable.ArrayBuffer[Vertex], width: Int, height: Int) {
  private val (pointsConvex, pointsNotInConvex) =
    ConvexHull.buildConvexHull(vertexes)

  private val triangles: mutable.Buffer[Triangle] =
    buildTrianglesInConvexHull(pointsConvex).toBuffer

  private val cache = new DynamicCache(triangles.head, 8)


  private def buildTrianglesInConvexHull(points: List[Vertex]) = {
    @tailrec
    def go(initialPoint: Vertex, points: List[Vertex], acc: List[Triangle]): List[Triangle] =points match {
      case b::c::Nil => Triangle(initialPoint, b, c) :: acc
      case b::c::tail => go(initialPoint, c::tail, Triangle(initialPoint, b, c) :: acc)
      case _ => acc
    }
    points match {
      case head::tail => go(head, tail, List.empty[Triangle])
      case _ => List.empty[Triangle]
    }
  }

  def ccw(a: Vertex, b: Vertex, c: Vertex): Double = ConvexHull.ccw(a, b, c)

  def angleProtiv(thisTriangle: Triangle, thatTriangle: Triangle): Unit  = {
    thisTriangle
      .getEdge(thatTriangle)
      .foreach { edge =>
        val thisVertex = (thisTriangle.vertexes.toBuffer - edge.a - edge.b).head
        val thatVertex = (thatTriangle.vertexes.toBuffer - edge.a - edge.b).head

        val (c, d) = if (ccw(thisVertex, edge.a, edge.b) < 0.0)
          (thatVertex, thisVertex) else (thisVertex, thatVertex)

        if (!Delone.checkDelaunayCondition(c, edge.a, d, edge.b)) {
          val newVertexesForThis = Array(edge.a, thisVertex, thatVertex).sorted
          val newVertexesForThat = Array(edge.b, thisVertex, thatVertex).sorted

          val neighbours = (thisTriangle.neigbourTriangles ++ thatTriangle.neigbourTriangles) diff Set(thisTriangle, thatTriangle)

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

          neighbours foreach { x =>
            if (thisTriangle.getEdge(x).isDefined)
              angleProtiv(x, thisTriangle)
            else
              angleProtiv(x, thatTriangle)
          }
        }
      }
  }


  def addVertexAndRebuild(vertex: Vertex): Unit = {
    val (x, y, triangleToStartSearch) = cache.findTriangle(vertex)
    val triangleToRebuild = triangleToStartSearch.findTrinagleForThatPoint(vertex)
    cache.setTriangleInCache(x, y, triangleToRebuild)

//    val triangleToRebuild = triangles.last.findTrinagleForThatPoint(vertex)

    if (triangleToRebuild.isDrawable) {
      def neigbours = triangleToRebuild.neigbourTriangles


      val firstTriangle = Triangle(vertex, triangleToRebuild.b, triangleToRebuild.c)
      val secondTriangle = Triangle(triangleToRebuild.a, vertex, triangleToRebuild.c)
      val thirdTriangle = Triangle(triangleToRebuild.a, triangleToRebuild.b, vertex)

      val trianglesToAdd = Array(firstTriangle, secondTriangle, thirdTriangle)

      triangles ++= trianglesToAdd
      triangleToRebuild.isDrawable = false

      trianglesToAdd.combinations(2).foreach(x => x(0).edgeWithOtherTriangle(x(1)))
      neigbours.foreach(x => trianglesToAdd.foreach(y => x.edgeWithOtherTriangle(y)))

      (trianglesToAdd ++ neigbours).combinations(2).foreach(x => angleProtiv(x(0), x(1)))
    }
  }

  def deloneAll(): Unit = pointsNotInConvex.tail.foreach(vertex => addVertexAndRebuild(vertex))

  def measuerTime(t: => Unit)(name : String) = {
    val before = System.currentTimeMillis()
    t
    val after = System.currentTimeMillis()
    println(s"$name ${after - before}")
  }

  def run(canva: Canvas) = {
    val before = System.currentTimeMillis()
    val combinations = triangles.combinations(2)


    measuerTime(combinations.foreach(x => x.head.edgeWithOtherTriangle(x(1))))("Edge")
    measuerTime(combinations.foreach(x => angleProtiv(x(0), x(1))))("Fuck")
    measuerTime(deloneAll())("Delone")
    println("It' okay")
    println()

//    triangles.filter(_.isDrawable) foreach (x => x.draw(canva))
  }
}
