import scalaj.http.Http

import java.net.URL
import scala.io.Source
import scala.util.{Try, Using}
import sys.env

trait Routines {
  case class Point(x: Int, y: Int) {
    def isAround(p: Point): Boolean = ((p.x - this.x).abs <= 1) && ((p.y - this.y).abs <= 1)

    def right: Point = Point(this.x + 1, this.y)

    def left: Point = Point(this.x - 1, this.y)

    def up: Point = Point(this.x, this.y + 1)

    def down: Point = Point(this.x, this.y - 1)
  }

  def withData(day: Int, debug: Boolean = false)(f : Iterator[String] => Unit): Try[Unit] = {
    val source = if (debug) {
      Source.fromFile(s"src/inputs/day$day.txt")
    } else {
      Source.fromString(
        Http(s"https://adventofcode.com/2022/day/$day/input")
        .header("Cookie", sys.env.getOrElse("AOC_COOKIE", ""))
        .header("User-Agent", "Scala AoC routines @vden").asString.body
      )
    }

    Using(source) { _data =>
      val data = _data.getLines()
      f(data)
    }
  }
}
