import utils.{AoC, Geometry}

import scala.util.Try
import scala.collection.mutable

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