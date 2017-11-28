package delone.cache

import geometry.{Triangle, Vertex}

trait Cacheable {
  def findTriangle(point: Vertex): Triangle
}
