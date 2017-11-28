package convexhull

import geometry.Vertex

import scala.annotation.tailrec

object ConvexHull {
  def ccw(a:Vertex, b:Vertex, c:Vertex): Double =
    (b.x - a.x)*(c.y - a.y) - (b.y - a.y)*(c.x - a.x)

  def traverse(points: List[Vertex]): List[Vertex] = {
    @tailrec
    def helper(points: List[Vertex], acc: List[Vertex]): List[Vertex] = points match {
      case Nil => acc
      case head :: tail => acc match {
        case last :: preLast :: _  if ccw(head, last, preLast) >= 0 =>
          helper(points, acc.tail)
        case _ =>
          helper(tail, head :: acc)
      }
    }
    if (points.length == 2) points
    else points match {
      case head :: next :: tail => helper(tail, next :: head :: Nil)
    }
  }

  def buildConvexHull(points: Seq[Vertex]): (List[Vertex], List[Vertex]) = {
    if (points.isEmpty) {
      List.empty[Vertex] -> List.empty[Vertex]
    }
    else {
      val buffer = points.toBuffer

      val before = System.currentTimeMillis()
      val pointsConvex = {
        val initialPoint = buffer.remove(buffer.indexOf(buffer.min))
        val pointsOrderedByPolarAngle = buffer.sortBy(point =>
          scala.math.atan2(point.y - initialPoint.y, point.x - initialPoint.x)
        )
        initialPoint :: traverse(pointsOrderedByPolarAngle.toList)
      }

      val pointsNotInConvex = buffer -- pointsConvex
      val after = System.currentTimeMillis()
      println(s"Convex ${after - before}")

      pointsConvex -> pointsNotInConvex.toList
    }
  }
}