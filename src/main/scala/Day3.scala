import utils.AoC


object Day3 extends AoC {
  def execute(day: Int) = {
    withData(day) { _data =>
      val (data1, data2) = _data.duplicate

      val sumPriorities = data1.map { line =>
        calculateScore(line.splitAt(line.length / 2).toList)
      }.sum
      println(s"Day 03, 1st part: $sumPriorities")

      val totalBadges = data2.grouped(3).map(calculateScore).sum
      println(s"Day 03, 2nd part: $totalBadges")
    }
  }

  def calculateScore(lines: Seq[String]) = {
    lines
      .map(_.toCharArray)
      .map(Set.from)
      .reduce { (acc, v) => acc & v }
      .map(priority)
      .head
  }

  def priority(n: Char) = {
    n.toByte match {
      case x if x > 96 => x - 96
      case x => x - 64 + 26
    }
  }
}

