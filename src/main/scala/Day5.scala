import scala.collection.mutable
import scala.util.Try

class Day5(inputFile: String) extends Routines {
  private case class Command(num: Int, from: Int, to: Int)

  private def parseCommand(line: String): Command = {
    val List(num, from, to) = line
      .split(" ")
      .filter(_.matches("\\d+"))
      .toList
    Command(num.toInt, from.toInt - 1, to.toInt - 1)
  }

  private def applyCommand9000(cargo: Seq[mutable.Stack[Char]], cmd: Command) = {
    (1 to cmd.num).foreach { _ =>
      cargo(cmd.to).push(cargo(cmd.from).pop())
    }
  }

  private def applyCommand9001(cargo: Seq[mutable.Stack[Char]], cmd: Command) = {
    (1 to cmd.num).map { _ => cargo(cmd.from).pop() }
      .reverse
      .foreach(cargo(cmd.to).push)
  }

  private def execute() = {
    withData(inputFile) { data =>

      val cargo: Vector[mutable.Stack[Char]] = data
        .takeWhile(_.contains('['))
        .map { line =>
          line.grouped(4).map { k =>
            if (k.startsWith(" ")) None else Some(k.charAt(1))
          }.toVector
        }
        .toVector
        .transpose
        .map(_.flatten)
        .map(mutable.Stack.from(_))

      val cargo2 = cargo.map(_.clone())

      data
        .dropWhile(x => !x.contains("move"))
        .map(parseCommand)
        .foreach { cmd =>
          applyCommand9000(cargo, cmd)
          applyCommand9001(cargo2, cmd)
        }

      val baseline1 = cargo.map(_.head).mkString("")
      println(s"Day 05, 1st part: $baseline1")

      val baseline2 = cargo2.map(_.head).mkString("")
      println(s"Day 05, 2nd part: $baseline2")
    }
  }
}

object Day5 {
  def apply(): Try[Unit] = {
    new Day5("src/inputs/day5.txt").execute()
  }
}