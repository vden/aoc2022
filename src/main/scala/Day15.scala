import utils.{AoC, Geometry}

object Day15 extends AoC with Geometry {
  def execute(day: Int) = {
    val p = "[0-9-]+".r

    withData(day) { data =>
      val signals = data.map { line =>
        p.findAllIn(line).map(_.toInt).grouped(2).map(x => Point(x.head, x.last)).toList
      }.toList

      val fields = signals.map { case List(s, b) => Diamond(s, s.distanceTo(b)) }

      val minX = fields.map(_.leftmost).min
      val maxX = fields.map(_.rightmost).max

      val checkLine = 2_000_000

      val xs = (minX to maxX).foldLeft(Set[Int]()) { (acc, x) =>
        val p = Point(x, checkLine)
        acc ++ fields.flatMap { f =>
          if f.hasPoint(p) then Some(x) else None
        }
      }

      val signalsAtLine = signals.map(_.last).toSet.count(_.y == checkLine)

      println(s"Day 15, 1st part: ${xs.size - signalsAtLine}")

      val posX = 4_000_000
      val posY = 4_000_000

      val distress = findBeacon(posX, posY, fields).get
      val tune: Long = distress.x * 4_000_000L + distress.y

      println(s"Day 15, 2nd part: $tune")
    }
  }

  def findBeacon(maxX: Int, maxY: Int, fields: Seq[Diamond]): Option[Point] = {
    for (y <- 0 to maxY) {
      var x = 0
      while (x <= maxX) {
        val p = Point(x, y)
        fields.find(_.hasPoint(p)) match {
          case Some(f) => x = f.skipByX(y)
          case None => return Some(p)
        }
        x += 1
      }
    }

    None
  }
}