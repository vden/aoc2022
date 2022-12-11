import scala.util.{Success, Failure}

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
  Day11() match {
    case Success(_) => ()
    case Failure(exception) =>
      println(s"ERR: $exception")
  }
}