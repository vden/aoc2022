import utils.{AoC, Geometry}

import scala.util.Try
import scala.collection.mutable

class Day9(day: Int) extends AoC with Geometry {
  private case class Move(dir: Char, num: Int)

  private def moveTail(point: Point, last: Point): Point = {
    if (!point.isAround(last)) {
      if (point.x == last.x) {
        if (point.y > last.y) last.up else last.down
      } else if (point.y == last.y) {
        if (point.x > last.x) last.right else last.left
      } else {
        (point.x > last.x, point.y > last.y) match {
          case (true, true) => last.right.up
          case (true, false) => last.right.down
          case (false, true) => last.left.up
          case (false, false) => last.left.down
        }
      }
    } else {
      last
    }
  }

  private def execute() = {
    withData(day) { data =>
      var head = Point(0, 0)
      val map: mutable.ArrayBuffer[Point] = mutable.ArrayBuffer.from(List(head))

      val moves = data
        .map { line =>
          val List(dir, num) = line.split(" ").toList
          Move(dir.charAt(0), num.toInt)
        }.toList

      moves
        .foreach { move =>
          head = (1 to move.num).foldLeft(head) { (acc, _) =>
            val newHead = move.dir match {
              case 'R' => acc.right
              case 'L' => acc.left
              case 'U' => acc.up
              case 'D' => acc.down
            }
            map += moveTail(newHead, map.last)
            newHead
          }
        }

      println(s"Day 09, 1st part: ${map.toSet.size}")

      map.clear()
      map += Point(0, 0)

      val knots: mutable.ArrayBuffer[Point] = mutable.ArrayBuffer.fill(10)(Point(0, 0))

      moves
        .foreach { move =>
          (1 to move.num).foreach { _ =>
            knots(0) = move.dir match {
              case 'R' => knots(0).right
              case 'L' => knots(0).left
              case 'U' => knots(0).up
              case 'D' => knots(0).down
            }

            val last = (1 to 9).foldLeft(knots(0)) { (acc, n) =>
              knots(n) = moveTail(acc, knots(n))
              knots(n)
            }
            map += last
          }
        }

      println(s"Day 09, 2nd part: ${map.toSet.size}")
    }
  }
}

object Day9 {
  def apply(): Try[Unit] = {
    new Day9(9).execute()
  }
}