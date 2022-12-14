import utils.{AoC, Geometry}

import scala.collection.mutable
import scala.util.Try

class Day14(day: Int) extends AoC with Geometry {
  extension (p: Point) {
    def downN: Point = Point(p.x, p.y + 1)
  }

  private def execute() = {
    withData(day) { data =>

      val walls = data
        .map(_.split(" -> ")
          .map(_.split(",").map(_.toInt).toList)
          .map(p => Point(p.head, p.last)).toList)
        .flatMap(_.foldLeft(Vector.empty[Point]) { (acc, v) =>
          acc match {
            case Vector() => Vector(v)
            case _ => acc ++ Line(acc.last, v).toPoints.drop(1)
          }
        })
        .toSet

      val points: mutable.HashSet[Point] = mutable.HashSet.from(walls)
      val bottomLine = walls.maxBy(_.y).y

      val source = Point(500, 0)
      var s = source
      while (s.y <= bottomLine) {
        s = source
        var settled = false
        while (!settled && s.y <= bottomLine) {
          List(s.downN, s.downN.left, s.downN.right)
            .flatMap { x =>
              if (!points.contains(x)) Some(x)
              else None
            }.headOption match {
            case None => settled = true
            case Some(x) => s = x
          }
        }
        points += s
      }

      val untilVoid = points.size - walls.size - 1
      println(s"Day $day, 1st part: $untilVoid")

      points.clear()
      points ++= walls
      val floor = bottomLine + 2

      while (s != source) {
        s = source
        var settled = false
        while (!settled) {
          List(s.downN, s.downN.left, s.downN.right)
            .flatMap { x =>
              if (!points.contains(x) && x.y != floor) Some(x)
              else None
            }.headOption match {
            case None => settled = true
            case Some(x) => s = x
          }
        }
        points += s
      }

      val sandPoints = points.size - walls.size
      println(s"Day $day, 2nd part: $sandPoints")
    }
  }
}

object Day14 {
  def apply(): Try[Unit] = {
    new Day14(14).execute()
  }
}