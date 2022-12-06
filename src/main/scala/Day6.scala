import scala.util.Try

class Day6(inputFile: String) extends Routines {
  private def findMark(size: Int)(data: String) = {
    data
      .view
      .sliding(size)
      .indexWhere { x => x.toSet.size == size } + size
  }

  private def execute() = {
    withData(inputFile) { _data =>
      val (data1, data2) = _data.duplicate

      val startOfPacket = data1.map(findMark(4)).toList.head
      println(s"Day 06, 1st part: $startOfPacket")

      val startOfMessage = data2.map(findMark(14)).toList.head
      println(s"Day 06, 2nd part: $startOfMessage")
    }
  }
}

object Day6 {
  def apply(): Try[Unit] = {
    new Day6("src/inputs/day6.txt").execute()
  }
}