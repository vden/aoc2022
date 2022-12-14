import utils.AoC

import scala.Ordered.orderingToOrdered
import scala.collection.mutable

object Day13 extends AoC {
  type Rec[Seq[_], A] = A match {
    case Int => Int | Seq[Rec[Seq, Int]]
    case _ => A | Seq[Rec[Seq, A]]
  }
  type Bit = Rec[Seq, Int]

  def execute(day: Int) = {
    withData(day) { data =>
      val items: Seq[(List[Bit], List[Bit])] = data
        .foldLeft(Seq[(Option[List[Bit]], Option[List[Bit]])]((None, None))) { (acc, line) =>
          line match {
            case "" => acc :+ (None, None)
            case d =>
              val parsed = parse(d.drop(1).dropRight(1))
              val elem = if (acc.last._1.isEmpty) then (Some(parsed), None) else (acc.last._1, Some(parsed))
              acc.dropRight(1) :+ elem
          }
        }
        .map(v => (v._1.get, v._2.get))

      val properPacketIDs = items
        .map(v => v._1 < v._2) // Ordered[Bit] implicitly derived from Ordering[Bit]
        .zipWithIndex
        .view
        .filter(_._1)
        .map(_._2 + 1)

      println(s"Day 13, 1st part: ${properPacketIDs.sum}")

      val dividers: List[Bit] = List(List(List(2)), List(List(6)))

      val full = items
        .foldLeft(dividers)((acc, v) => acc :++ v.toList)
        .sorted // (using bitOrdering)

      val decoderKey = full
        .zipWithIndex
        .view
        .filter((x, _) => dividers.contains(x))
        .map(_._2 + 1)
        .product

      println(s"Day 13, 2nd part: $decoderKey")
    }
  }

  private def parse(chars: Seq[Char]): List[Bit] = {
    def parseInternal(chars: Seq[Char], start: Int): (List[Bit], Int) = {
      val line: mutable.ArrayBuffer[Bit] = mutable.ArrayBuffer.empty
      var exit = false
      var idx = start
      while (idx < chars.length && !exit) {
        chars(idx) match {
          case c if c.isDigit =>
            val s = chars.drop(idx + 1).takeWhile(_.isDigit).foldLeft(c.toString)((acc, v) => acc + v)
            line += s.toInt
            idx += (s.length - 1)
          case '[' =>
            val (elem, newIdx) = parseInternal(chars, idx + 1)
            line += elem
            idx = newIdx
          case ']' =>
            exit = true
          case ',' =>
        }
        idx += 1
      }
      (line.toList, idx - 1)
    }

    parseInternal(chars, 0)._1
  }

  given Ordering[Bit] with {
    override def compare(x: Bit, y: Bit): Int =
      (x, y) match {
        case (a: Int, b: Int) =>
          if (a == b) then 0 else if (a > b) then 1 else -1
        case (a: Int, b: Seq[Int]) =>
          compare(Seq(a), b)
        case (a: Seq[Int], b: Int) =>
          compare(a, Seq(b))
        case (a: Seq[Bit], b: Seq[Bit]) =>
          val r = a.zipAll(b, None, None).map {
            case (None, _) => -1
            case (_, None) => 1
            case (a: Bit, b: Bit) => compare(a, b)
          }.dropWhile(_ == 0)
          if (r.isEmpty) then 0 else r.head
      }
  }
}