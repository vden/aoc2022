import utils.AoC

import java.util.regex.Pattern
import scala.util.Try
import scala.collection.mutable

class Day11(day: Int) extends AoC {

  import Day11._

  private def execute() = {
    withData(day) { _data =>
      val (data1, data2) = _data.duplicate

      val monkeys = describeMonkeys(data1)
      val inspections: mutable.Map[Int, Int] = mutable.Map.empty.withDefaultValue(0)

      for (_ <- 0 until 20) {
        monkeys.zipWithIndex.foreach { (m, idx) =>
          m.items.foreach { i =>
            inspections(idx) += 1
            val item = Item((m.op(i.worry) / 3.0).floor.toLong)
            val to = m.passTo(item)
            monkeys(to).items += item
          }
          m.items.clear()
        }
      }

      val topOfMonkeyBusiness = inspections.toList.map(_._2).sorted.reverse.take(2).product
      println(s"Day 11, 1st part: $topOfMonkeyBusiness")

      val monkeysLong = describeMonkeys(data2)
      val lcm = monkeysLong.map(_.test).product
      inspections.clear()

      for (n <- 0 until 10_000) {
        // if (n % 1000 == 0 || n == 20) { print(s"Round $n: "); println(inspections) }

        monkeysLong.zipWithIndex.foreach { (m, idx) =>
          m.items.foreach { i =>
            inspections(idx) += 1
            val item = Item(m.op(i.worry) % lcm)
            val to = m.passTo(item)
            monkeysLong(to).items += item
          }
          m.items.clear()
        }
      }

      val topOfMonkeyBusinessLong = inspections.toList.map(_._2).sorted.reverse.take(2).map(_.toLong).product
      println(s"Day 11, 2nd part: $topOfMonkeyBusinessLong")
    }
  }

  private def describeMonkeys(data: Iterator[String]) = {
    val pMonkey = "Monkey (\\d+):".r
    val pItems = ".+?items: ([0-9, ]+)".r
    val pOp = ".+Operation:.+= old (.) (.+)".r
    val pTest = ".+by (\\d+)".r
    val pCond = ".+?throw to monkey (\\d+)".r

    data
      .foldLeft(List(MonkeyBuilder())) { (acc, line) =>
        if (line == "") {
          acc :+ MonkeyBuilder()
        } else {
          val builder = line match {
            case pMonkey(_) => acc.last
            case pItems(items) =>
              acc.last.withItems(items.split(", ").map(x => Item(x.trim.toInt)).toSeq)
            case pOp(op, arg) =>
              val newOp = op match {
                case "*" if arg == "old" => (x: Long) => x * x
                case "*" => (x: Long) => x * arg.toLong
                case "+" => (x: Long) => x + arg.toLong
              }
              acc.last.withOp(newOp)
            case pTest(test) => acc.last.withTest(test.toInt)
            case pCond(m) => acc.last.withDecisions(acc.last.decisions :+ m.toInt)
          }

          acc.dropRight(1) :+ builder
        }
      }
      .map(_.build)
  }
}

object Day11 {
  case class Item(worry: Long)

  case class Monkey(items: mutable.ArrayBuffer[Item], op: Long => Long, test: Int, decisions: List[Int]) {

    def passTo(i: Item): Int = {
      if (i.worry % this.test == 0) decisions.head else decisions.last
    }
  }

  case class MonkeyBuilder(items: Seq[Item] = Seq.empty,
                           op: Long => Long = (_) => 0,
                           test: Int = 0,
                           decisions: List[Int] = List.empty) {
    def withOp(newOp: Long => Long) = this.copy(op = newOp)

    def withItems(newItems: Seq[Item]) = this.copy(items = newItems)

    def withTest(newTest: Int) = this.copy(test = newTest)

    def withDecisions(newDecisions: List[Int]) = this.copy(decisions = newDecisions)

    def build = Monkey(mutable.ArrayBuffer(this.items: _*), this.op, this.test, this.decisions)
  }

  def apply(): Try[Unit] = {
    new Day11(11).execute()
  }
}