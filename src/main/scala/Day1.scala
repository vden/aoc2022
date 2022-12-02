import scala.util.Try

class Day1(val inputFile: String) extends Routines {
  private def execute(): Try[Unit] = {
    withData(inputFile) { data =>
      val rations = data.foldLeft(List[Int](0))((acc: List[Int], v: String) => {
        v match {
          case "" => 0 +: acc
          case x => (acc.head + x.toInt) +: acc.tail
        }
      })

      println(s"Day 01, 1st part: ${rations.max}")

      val totalThreeTop = rations.sorted(using Ordering[Int].reverse).take(3).sum

      println(s"Day 01, 2nd part: $totalThreeTop")
    }
  }
}

object Day1 {
  def apply(): Try[Unit] = {
    new Day1("src/inputs/day1.txt").execute()
  }
}