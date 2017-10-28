package convexhull

import delone.Vertex

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer


object ConvexHull {
  def crossProduct(a:Vertex, b:Vertex, c:Vertex): Double =
    (b.x - a.x)*(c.y - a.y) - (b.y - a.y)*(c.x - a.x)

  def traverse(points: List[Vertex]): List[Vertex] = {
    @tailrec
    def helper(points: List[Vertex], acc: List[Vertex]): List[Vertex] = points match {
      case Nil => acc
      case head :: tail => acc match {
        case last :: preLast :: stack => if (crossProduct(head, last, preLast) >= 0)
          helper(points, acc.tail)
        else helper(tail, head :: acc)
        case _ => helper(tail, head :: acc)
      }
    }
    if (points.length == 2) points
    else points match {
      case head :: next :: tail => helper(tail, next :: head :: Nil)
    }
  }

  def buildConvexHull(points: ArrayBuffer[Vertex]): (List[Vertex], List[Vertex]) = {
    val pointsConvex = {
      val initialPoint = points.remove(points.indexOf(points.min))
      val pointsOderedByPolarAngle = points
        .sortBy(point => scala.math.atan2(point.y - initialPoint.y, point.x - initialPoint.x))
      initialPoint :: traverse((pointsOderedByPolarAngle.+=:(initialPoint)).toList.tail)
    }

    val pointsNotInConvex = points -- pointsConvex
    (pointsConvex, pointsNotInConvex.toList)
  }
}