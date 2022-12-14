package utils

import scalaj.http.Http

import java.net.URL
import scala.io.Source
import scala.sys.env
import scala.util.{Try, Using}

trait AoC {
  def execute(day: Int): Try[Unit]

  def withData(day: Int, debug: Boolean = false)(f: Iterator[String] => Unit): Try[Unit] = {
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
