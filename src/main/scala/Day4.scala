import utils.AoC

object Day4 extends AoC {
  def execute(day: Int) = {
    withData(day) { _data =>
      val (data1, data2) = _data.duplicate

      val countDuplicate = data1
        .map(processLine)
        .count(x => containsRange(x(0), x(1)))

      println(s"Day 04, 1st part: $countDuplicate")

      val countOverlaps = data2
        .map(processLine)
        .count(x => overlapsRange(x(0), x(1)))

      println(s"Day 04, 2nd part: $countOverlaps")
    }
  }

  private def containsRange(a: Range, b: Range): Boolean = {
    (a.start <= b.start && b.end <= a.end) || (b.start <= a.start && a.end <= b.end)
  }

  private def overlapsRange(a: Range, b: Range): Boolean = a.intersect(b).nonEmpty

  private def processLine(line: String) = {
    line
      .split(",")
      .map(_.split("-"))
      .map(x => Range.inclusive(x(0).toInt, x(1).toInt))
  }
}
