import scala.io.Source
import scala.util.{Try, Using}

trait Routines {
  def withData(path: String)(f : Iterator[String] => Unit): Try[Unit] = {
    Using(Source.fromFile(path)) { _data =>
      val data = _data.getLines()
      f(data)
    }
  }
}
