import utils.{AoC, Geometry}

import scala.collection.mutable
import scala.util.Try

class Day12(day: Int) extends AoC with Geometry {

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
          }.toVector
        }.toVector
        .transpose

      def cond(to: Int, from: Int) = to - from <= 1

      def cost(x: Point, y: Point) = 1

      val path = A_Star(List(head), end, map, cond, cost)
      println(s"Day 12, 1st part: ${path.size - 1}")

      val starts = map.zipWithIndex.flatMap { (col, x) =>
        col.zipWithIndex.flatMap { (v, y) =>
          if (v == 0) Some(Point(x, y)) else None
        }
      }

      val path2 = A_Star(starts, end, map, cond, cost)
      println(s"Day 12, 2nd part: ${path2.size - 1}")
    }
  }
}

object Day12 {
  def apply(): Try[Unit] = {
    new Day12(12).execute()
  }
}