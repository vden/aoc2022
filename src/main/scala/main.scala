import scala.util.{Failure, Success}

@main
def main(): Unit = {
  // Day1()
  // Day2()
  // Day3()
  // Day4()
  // Day5()
  // Day6()
  // Day7()
  // Day8()
  // Day9()
  // Day10()
  // Day11()
  // Day12()
  // Day13()
  Day14() match {
    case Success(_) => ()
    case Failure(exception) =>
      println(s"ERR: $exception")
      println(exception.getStackTrace.mkString("\n"))
  }
}