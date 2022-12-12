import utils.AoC

import scala.util.Try

class Day2(day: Int) extends AoC {
  enum Throw {
    case Rock, Paper, Scissors

    def score() = this.ordinal + 1

    def beats(them: Throw): Option[Boolean] = {
      if (this == them) None
      else if ((this.ordinal - them.ordinal).abs == 1) {
        Some(this.ordinal > them.ordinal)
      } else {
        Some(this.ordinal < them.ordinal)
      }
    }

    def loseTo(): Throw = {
      if (this == Scissors) Rock
      else Throw.fromOrdinal(this.ordinal + 1)
    }

    def winsFrom(): Throw = {
      if (this == Rock) Scissors
      else Throw.fromOrdinal(this.ordinal - 1)
    }
  }

  object Throw {
    def apply(code: String): Throw = {
      code match {
        case "X" | "A" => Rock
        case "Y" | "B" => Paper
        case _ => Scissors
      }
    }
  }

  enum Result {
    case Lose, Draw, Win
  }
  object Result {
    def apply(code: String): Result = {
      code match {
        case "X" => Lose
        case "Y" => Draw
        case _ => Win
      }
    }
  }

  private def evalGame(them: Throw, me: Throw): Int = {
    me.beats(them) match {
      case None => me.score() + 3
      case Some(x) =>
        me.score() + (if x then 6 else 0)
    }
  }

  // 1st part to check throws
  private def checkThrow(line: String): Int = {
    val Seq(them, me) = line.split(" ").map(Throw.apply).toSeq
    evalGame(them, me)
  }

  // 2nd part to get the result needed
  private def makeThrow(line: String): Int = {
    val f = line.split(" ")
    val them = Throw(f(0))
    val ending = Result(f(1))

    val me = ending match {
      case Result.Lose => them.winsFrom()
      case Result.Win => them.loseTo()
      case Result.Draw => them
    }

    evalGame(them, me)
  }

  private def execute() = {
    withData(day) { _data =>
      val (data1, data2) = _data.duplicate

      val s = data1.map(checkThrow).sum
      println(s"Day 02, 1st part: $s")

      val s1 = data2.map(makeThrow).sum
      println(s"Day 02, 2nd part: $s1")
    }
  }
}

object Day2 {
  def apply(): Try[Unit] = {
    new Day2(2).execute()
  }
}