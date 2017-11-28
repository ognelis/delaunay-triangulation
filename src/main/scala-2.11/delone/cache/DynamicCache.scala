package delone.cache

import geometry.{Triangle, Vertex}

class DynamicCache(triangle: Triangle, dynamicCacheCoefficient: Int) {
  private var cache = Array.fill(2, 2)(triangle)
  private var cacheExtensionCondition = 0L

  private def checkCacheConditionGrow(): Boolean = {
    cacheExtensionCondition = cacheExtensionCondition + 1L
    val m = cache.length
    val r = dynamicCacheCoefficient
    cacheExtensionCondition >= m * m * r
  }

  private def resizeCache(): Unit = {
    val nrow = cache.length

    val newCacheSize = nrow*2
    val newCache = Array.ofDim[Triangle](newCacheSize, newCacheSize)

    for {
      i <- cache.indices
      j <- cache.indices
      triangle = cache(i)(j)
    } yield {

      val iNewCache = 2*i
      val jNewCache = 2*j

      newCache(iNewCache)(jNewCache)         = triangle
      newCache(iNewCache)(jNewCache + 1)     = triangle
      newCache(iNewCache + 1)(jNewCache)     = triangle
      newCache(iNewCache + 1)(jNewCache + 1) = triangle

      cache(i)(j) = null
    }
    cache = newCache
  }

  def findTriangle(vertex: Vertex): (Int, Int, Triangle) = {
    if (checkCacheConditionGrow()) resizeCache()

    val m = cache.length
    val x = (vertex.x % m).toInt
    val y = (vertex.y % m).toInt

    val triangle = cache(x)(y)
    (x, y, triangle)
  }

  def setTriangleInCache(x: Int, y: Int, triangle: Triangle): Unit = cache(x)(y) = triangle
}
