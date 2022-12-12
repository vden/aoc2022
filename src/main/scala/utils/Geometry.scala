package utils

import scala.collection.mutable

trait Geometry {
  case class Point(x: Int, y: Int) {
    def isAround(p: Point): Boolean = ((p.x - this.x).abs <= 1) && ((p.y - this.y).abs <= 1)

    def right: Point = Point(this.x + 1, this.y)

    def left: Point = Point(this.x - 1, this.y)

    def up: Point = Point(this.x, this.y + 1)

    def down: Point = Point(this.x, this.y - 1)
  }

  def A_Star[T](starts: Seq[Point], end: Point, map: Vector[Vector[T]],
                condMap: (T, T) => Boolean, edgeCost: (Point, Point) => Int): Seq[Point] = {
    def score(from: Point) = (from.x - end.x).abs + (from.y - end.y).abs

    def reconstruct(cameFrom: Map[Point, Point], current: Point) = {
      val path: mutable.ArrayBuffer[Point] = mutable.ArrayBuffer.from(List(current))
      while (cameFrom.contains(path.head)) {
        path.prepend(cameFrom(path.head))
      }
      path.toSeq
    }

    val openSet: mutable.PriorityQueue[Point] = mutable.PriorityQueue.from(starts)(Ordering.by(score))

    val cameFrom: mutable.HashMap[Point, Point] = mutable.HashMap.empty

    val gScore: mutable.Map[Point, Int] = mutable.HashMap.empty.withDefaultValue(Int.MaxValue)
    val fScore: mutable.Map[Point, Int] = mutable.HashMap.empty.withDefaultValue(Int.MaxValue)

    starts.foreach { s =>
      gScore(s) = 0
      fScore(s) = score(s)
    }

    var current = starts.head
    while (openSet.nonEmpty) {
      current = openSet.dequeue()
      if (current == end) {
        return reconstruct(cameFrom.toMap, current)
      }

      List(current.up, current.right, current.down, current.left).foreach { p =>
        val outOfMap = (p.x < 0 || p.y < 0 || p.x >= map.size || p.y >= map.head.size)
        if (!outOfMap) {
          val outOfHeight = !condMap(map(p.x)(p.y), map(current.x)(current.y))
          if (!outOfHeight) {
            val tentative = gScore(current) + edgeCost(current, p)
            if (tentative < gScore(p)) {
              cameFrom(p) = current
              gScore(p) = tentative
              fScore(p) = tentative + score(p)
              if (!openSet.exists(_ == p)) {
                openSet.enqueue(p)
              }
            }
          }
        }
      }
    }

    Seq()
  }
}
