import utils.AoC

import scala.util.{Failure, Success}

@main
def main(): Unit = {
  // for (i <- 1 to AoC_2022.days.length) { AoC_2022.day(i) }

  AoC_2022.day(15)
}

object AoC_2022 {
  val days: List[AoC] = List(Day1, Day2, Day3, Day4, Day5, Day6,
    Day7, Day8, Day9, Day10, Day11, Day12, Day13, Day14, Day15)

  def day(n: Int): Unit = {
    days(n - 1).execute(n) match {
      case Success(_) => ()
      case Failure(exception) =>
        println(s"ERR: $exception")
        println(exception.getStackTrace.mkString("\n"))
    }
  }
}