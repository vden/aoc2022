import utils.AoC

import scala.collection.mutable

object Day18 extends AoC {

  def execute(day: Int) = {
    withData(day) { data =>
      val cubes = data
        .map(_.split(",").map(_.toInt))
        .map { case Array(x, y, z) => (x, y, z) }.toList

      val allSurface = cubes.map { cube => edges(cube).count(x => !cubes.contains(x)) }.sum

      println(s"Day 18, 1st part: $allSurface")

      val (minX, minY, minZ) = (-1, -1, -1)
      val maxX = cubes.map(_._1).max + 1
      val maxY = cubes.map(_._2).max + 1
      val maxZ = cubes.map(_._3).max + 1

      def floodFill(s: (Int, Int, Int)): Int = {
        val open: mutable.Queue[(Int, Int, Int)] = mutable.Queue.from(List(s))
        val seen: mutable.HashSet[(Int, Int, Int)] = mutable.HashSet.from(List(s))

        val marked: mutable.HashSet[(Int, Int, Int)] = mutable.HashSet.from(List(s))

        while (open.nonEmpty) {
          val current = open.dequeue()

          edges(current).foreach { c =>
            if (!seen.contains(c)) {
              seen += c
              if ((minX <= c._1) && (minY <= c._2) && (minZ <= c._3) &&
                (c._1 <= maxX) && (c._2 <= maxY) && (c._3 <= maxZ) &&
                !cubes.contains(c)) {
                marked += c
                open.enqueue(c)
              }
            }
          }
        }

        cubes.map { c => edges(c).count(marked.contains) }.sum
      }

      val totalWithoutGaps = floodFill((-1, -1, -1))
      println(s"Day 18, 2nd part: $totalWithoutGaps")
    }
  }

  def edges(p: (Int, Int, Int)) = {
    List((-1, 0, 0), (1, 0, 0), (0, -1, 0), (0, 1, 0), (0, 0, -1), (0, 0, 1)).map { x =>
      (p._1 + x._1, p._2 + x._2, p._3 + x._3)
    }
  }
}
