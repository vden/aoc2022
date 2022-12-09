import scala.io.Source
import scala.util.{Try, Using}

trait Routines {
  case class Point(x: Int, y: Int) {
    def isAround(p: Point): Boolean = ((p.x - this.x).abs <= 1) && ((p.y - this.y).abs <= 1)

    def right: Point = Point(this.x + 1, this.y)

    def left: Point = Point(this.x - 1, this.y)

    def up: Point = Point(this.x, this.y + 1)

    def down: Point = Point(this.x, this.y - 1)
  }

  def withData(path: String)(f : Iterator[String] => Unit): Try[Unit] = {
    Using(Source.fromFile(path)) { _data =>
      val data = _data.getLines()
      f(data)
    }
  }
}
