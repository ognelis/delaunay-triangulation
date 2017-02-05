package delone

/**
  * Created by Admin on 28.11.2016.
  */
case class Edge(a: Vertex, b: Vertex) {
  override def equals(obj: scala.Any): Boolean = obj match {
    case that: Edge =>
      this.a == that.a && this.b == that.b
    case _ => false
  }
  var triangle2: Triangle = _
}