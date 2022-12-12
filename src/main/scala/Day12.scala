import scala.util.Try
import scala.collection.mutable

class Day12(day: Int) extends Routines {

  def reconstruct(cameFrom: Map[Point, Point], current: Point) = {
    val path: mutable.ArrayBuffer[Point] = mutable.ArrayBuffer.from(List(current))
    while (cameFrom.contains(path.head)) {
      path.prepend(cameFrom(path.head))
    }
    path.toSeq
  }

  def A_Star[T](start: Point, end: Point, map: List[List[T]],
                condMap: (T, T) => Boolean, edgeCost: (Point, Point) => Int): Seq[Point] = {
    def score(from: Point) = (from.x - end.x).abs + (from.y - end.y).abs

    val openSet: mutable.PriorityQueue[Point] = mutable.PriorityQueue.from(List(start))(Ordering.by(score))

    val cameFrom: mutable.HashMap[Point, Point] = mutable.HashMap.empty

    val gScore: mutable.Map[Point, Int] = mutable.HashMap.empty.withDefaultValue(Int.MaxValue)
    gScore(start) = 0

    val fScore: mutable.Map[Point, Int] = mutable.HashMap.empty.withDefaultValue(Int.MaxValue)
    fScore(start) = score(start)

    var current = start
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

  private def execute() = {
    withData(day) { data =>
      var head: Point = Point(-1, -1)
      var end: Point = Point(-1, -1)

      val map = data
        .zipWithIndex
        .map { (line, y) =>
          line.zipWithIndex.map { (char, x) =>
            char match {
              case 'S' => head = Point(x, y); 0
              case 'E' => end = Point(x, y); 'z' - 97
              case c => c - 97
            }
          }.toList
        }.toList
        .transpose

      def cond(to: Int, from: Int) = to - from <= 1

      def cost(x: Point, y: Point) = 1

      val path = A_Star(head, end, map, cond, cost)
      println(s"Day 12, 1st part: ${path.size - 1}")

      val starts = map.zipWithIndex.flatMap { (col, x) =>
        col.zipWithIndex.flatMap { (v, y) =>
          if (v == 0) Some(Point(x, y)) else None
        }
      }

      val paths = starts.map(s => A_Star(s, end, map, cond, cost).size - 1).filter(_ > 0)
      println(s"Day 12, 2nd part: ${paths.min}")
    }
  }
}

object Day12 {
  def apply(): Try[Unit] = {
    new Day12(12).execute()
  }
}