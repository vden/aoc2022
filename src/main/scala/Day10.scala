import utils.AoC

import scala.collection.mutable

object Day10 extends AoC {
  def execute(day: Int) = {
    withData(day) { data =>
      val state: mutable.ArrayBuffer[Int] = mutable.ArrayBuffer.from(List(1))

      data
        .foreach { line =>
          line.split(" ") match {
            case Array("noop") => state += state.last
            case Array("addx", arg: String) =>
              state ++= Array(state.last, state.last + arg.toInt)
          }
        }

      val sumOfStrengths = (20 to state.size by 40).foldLeft(0) { (acc, n) =>
        state(n - 1) * n + acc
      }
      println(s"Day 10, 1st part: $sumOfStrengths")

      println("Day 10, 2nd part:")
      state.dropRight(1).zipWithIndex.foreach { (x, cycle) =>
        val sx = cycle % 40
        if ((x - sx).abs <= 1) print('#') else print('.')
        if ((cycle + 1) % 40 == 0) println()
      }
    }
  }
}