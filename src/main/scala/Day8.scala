import utils.AoC

import scala.collection.mutable
import scala.util.Try

class Day8(day: Int) extends AoC {
  private def execute() = {
    withData(day) { data =>
      val trees = data
        .map(_.map(_.toString.toInt).toVector).toVector
        .transpose

      val dimX = trees.length
      val dimY = trees.head.length

      val visible: mutable.HashSet[(Int, Int)] = mutable.HashSet.empty

      List(0, dimX - 1).foreach { x =>
        (0 until dimY).foreach { y =>
          visible += ((x, y))
        }
      }

      List(0, dimY - 1).foreach { y =>
        (0 until dimX).foreach { x =>
          visible += ((x, y))
        }
      }

      (1 until dimX).foreach { x =>
        var top = trees(x)(0)
        for (y <- 1 until dimY; if top < 9) {
          if (trees(x)(y) > top) {
            visible += ((x, y))
            top = trees(x)(y)
          }
        }
      }

      (1 until dimX).foreach { x =>
        var top = trees(x)(dimY - 1)
        for (y <- (dimY - 2) until 0 by -1; if top < 9) {
          if (trees(x)(y) > top) {
            visible += ((x, y))
            top = trees(x)(y)
          }
        }
      }

      (1 until dimY).foreach { y =>
        var top = trees(0)(y)
        for (x <- 1 until dimX; if top < 9) {
          if (trees(x)(y) > top) {
            visible += ((x, y))
            top = trees(x)(y)
          }
        }
      }

      (1 until dimY).foreach { y =>
        var top = trees(dimX - 1)(y)
        for (x <- (dimX - 2) until 0 by -1; if top < 9) {
          if (trees(x)(y) > top) {
            visible += ((x, y))
            top = trees(x)(y)
          }
        }
      }

      println(s"Day 08, 1st part: ${visible.size}")

      val points: mutable.ArrayBuffer[Int] = mutable.ArrayBuffer.empty

      (1 until (dimX - 1)).foreach { x =>
        (1 until (dimY - 1)).foreach { y =>
          val total = mutable.ArrayBuffer.from(List(0, 0, 0, 0))

          val height = trees(x)(y)

          var nx = x - 1
          var stop = false
          while (nx >= 0 && !stop) {
            total(0) += 1
            if (trees(nx)(y) >= height) stop = true
            nx -= 1
          }

          nx = x + 1
          stop = false
          while (nx < dimX && !stop) {
            total(1) += 1
            if (trees(nx)(y) >= height) stop = true
            nx += 1
          }

          var ny = y - 1
          stop = false
          while (ny >= 0 && !stop) {
            total(2) += 1
            if (trees(x)(ny) >= height) stop = true
            ny -= 1
          }

          ny = y + 1
          stop = false
          while (ny < dimY && !stop) {
            total(3) += 1
            if (trees(x)(ny) >= height) stop = true
            ny += 1
          }

          // println(s"$x,$y ($height): $total, ${total.product}")
          points += total.product
        }
      }

      println(s"Day 08, 2nd part: ${points.max}")
    }
  }
}

object Day8 {
  def apply(): Try[Unit] = {
    new Day8(8).execute()
  }
}
