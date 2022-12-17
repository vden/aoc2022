import utils.AoC

import scala.collection.mutable
import scala.util.control.Breaks.{break, breakable}


object Day17 extends AoC {
  def execute(day: Int) = {
    withData(day) { data =>
      val Rock1 = Vector[Byte](30) // 0x0011110
      val Rock2 = Vector[Byte](8, 28, 8) // 0b0001000, 0b0011100, 0b0001000
      val Rock3 = Vector[Byte](28, 4, 4) // 0b0011100, 0b0000100, 0b0000100
      val Rock4 = Vector[Byte](16, 16, 16, 16) // 0b0010000, 0b0010000, 0b0010000, 0b0010000
      val Rock5 = Vector[Byte](24, 24) // 0b0011000, 0b0011000

      val jets = data.map(_.toCharArray.toVector).toList.head
      val jetsLength = jets.length

      val rocks = List(Rock1, Rock2, Rock3, Rock4, Rock5)

      def findTotalHeight(maxRocks: Int): (Int, Int) = {

        val states: mutable.HashMap[Int, (Int, Int, Int)] = mutable.HashMap.empty
        val pit: mutable.ArrayBuffer[Byte] = mutable.ArrayBuffer.from(List(127, 0, 0, 0))

        def isBlocked(c: Vector[Byte], h: Int): Boolean = {
          c.zipWithIndex.exists { (x, idx) =>
            if (h + idx >= pit.length) false else (pit(h + idx) & x) > 0
          }
        }

        var cycleRock = 0
        var height = 4
        var rockCounter = 0
        var jetsCounter = 0
        var current = rocks.head

        breakable {
          while (true) {
            val j = jets(jetsCounter % jetsLength)
            if (rockCounter >= maxRocks) break

            j match {
              case '>' => if (!current.exists(x => (x & 1) == 1)) {
                val c = current.map(_ >> 1).map(_.toByte)
                if (!isBlocked(c, height)) current = c
              }
              case '<' => if (!current.exists(x => (x & 64) == 64)) {
                val c = current.map(_ << 1).map(_.toByte)
                if (!isBlocked(c, height)) current = c
              }
            }

            height -= 1

            if (isBlocked(current, height)) {
              rockCounter += 1

              for (i <- (height + 1) to (height + current.length)) {
                val rel = i - height - 1
                if (i >= pit.length) pit += current(rel) else pit(i) = (pit(i) | current(rel)).toByte
              }

              val highest = pit.view.takeWhile(_ > 0).size

              current = rocks(rockCounter % 5)
              height = highest + 3

              (pit.length until (height)).foreach(_ => pit += 0)

              if (pit.length > 20) {
                val stateFp = (pit(highest - 1), rockCounter % 5, jetsCounter % jetsLength)

                for (i <- states.keys) {
                  if (stateFp == states(i)) cycleRock = rockCounter - i
                }
              }

              states += (rockCounter -> (pit(highest - 1), rockCounter % 5, jetsCounter % jetsLength))
            }

            jetsCounter += 1
          }
        }

        (pit.view.takeWhile(_ > 0).size - 1, cycleRock)
      }

      val (totalHeight2022, cycle) = findTotalHeight(2022)
      println(s"Cycle is $cycle")

      println(s"Day 17, 1st part: $totalHeight2022")

      val total = 1_000_000_000_000L
      val rest = total % cycle
      val cycleHeight = findTotalHeight(cycle * 2)._1 - findTotalHeight(cycle)._1
      val totalHeightFull = cycleHeight * (total / cycle) + findTotalHeight(rest.toInt)._1
      println(s"Day 17, 2nd part: $totalHeightFull")
    }
  }

  def printB(n: Int) = {
    n.toBinaryString.reverse.padTo(7, '0').take(7).reverse
      .replace('0', '.').replace('1', '#')
  }
}
