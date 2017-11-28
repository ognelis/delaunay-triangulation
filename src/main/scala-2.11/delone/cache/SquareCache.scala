package delone.cache

import geometry.{Triangle, Vertex}

import scala.annotation.tailrec

private[cache] object Square {
  @tailrec
  final def findTriangle(square: Square, point: Vertex): Triangle = square match {
    case x: NodeSquare => x.triangle
    case x: LeafSquare => findTriangle(x.chooseSquare(point), point)
  }
}

private[cache] abstract sealed class Square(leftTopCorner: Vertex, rightBottomCorner: Vertex)

private[cache] case class NodeSquare(leftTopCorner: Vertex, rightBottomCorner: Vertex, triangle: Triangle)
  extends Square(leftTopCorner, rightBottomCorner)

private[cache] case class LeafSquare(leftTopCorner: Vertex,
                                     rightBottomCorner: Vertex,
                                     var topLeft: Square,
                                     var topRight: Square,
                                     var bottomLeft: Square,
                                     var bottomRight: Square) extends Square(leftTopCorner, rightBottomCorner) {
  def chooseSquare(vertex: Vertex): Square = {
    val middleX = (leftTopCorner.x + rightBottomCorner.x) / 2
    val middleY = (leftTopCorner.y + rightBottomCorner.y) / 2

    (vertex.x <= middleX) -> (vertex.y <= middleY) match {
      case (true, true) => topLeft
      case (true, false) => bottomLeft
      case (false, true) => topRight
      case _ => bottomRight
    }
  }
}



