import utils.AoC


object Day6 extends AoC {
  def execute(day: Int) = {
    withData(day) { _data =>
      val (data1, data2) = _data.duplicate

      val startOfPacket = data1.map(findMark(4)).toList.head
      println(s"Day 06, 1st part: $startOfPacket")

      val startOfMessage = data2.map(findMark(14)).toList.head
      println(s"Day 06, 2nd part: $startOfMessage")
    }
  }

  private def findMark(size: Int)(data: String) = {
    data
      .view
      .sliding(size)
      .indexWhere { x => x.toSet.size == size } + size
  }
}
