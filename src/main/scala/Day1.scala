import utils.AoC

object Day1 extends AoC {
  def execute(day: Int) = {
    withData(day) { data =>
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
