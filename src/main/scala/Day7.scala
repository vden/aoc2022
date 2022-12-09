import scala.util.Try
import scala.collection.mutable

class Day7(day: Int) extends Routines {

  private class Node(var size: Int) {
    val nodes: mutable.Map[String, Node] = mutable.Map.empty

    def printNodes(pad: Int): Unit = {
      println(s" $size")
      nodes.foreach { (k, v) =>
        print(k.reverse.padTo(k.length + pad, '-').reverse)
        v.printNodes(pad + 2)
      }
    }

    def filterNodeValues(cond: => (Int => Boolean)): Seq[Int] = {
      nodes.flatMap((_, v) => v.filterNodeValues(cond)).toSeq ++
      nodes
        .filter((_, v) => cond(v.size))
        .map((_, v) => v.size)
        .toSeq
    }
  }

  private class Tree {
    val head: Node = new Node(0)

    def printTree(): Unit = {
      print("/")
      head.printNodes(0)
    }

    def filterTreeValues(cond: => (Int => Boolean)): Seq[Int] = head.filterNodeValues(cond)
  }

  private def updatePath(path: Array[String], dir: String) = {
    dir match {
      case ".." => path.dropRight(1)
      case "/" => Array("")
      case x => path :+ x
    }
  }

  private def execute() = {
    withData(day) { data =>

      var cwd: Array[String] = Array.empty
      val tree = new Tree()

      for (line <- data) {
        val tokens = line.split(" ")
        tokens.head match {
          case "$" =>
            tokens.tail match {
              case Array("cd", x) => cwd = updatePath(cwd, x)
              case Array("ls") => ()
            }
          case "dir" =>
            val leaf = cwd.drop(1).foldLeft(tree.head) { (node, p) => node.nodes(p) }
            leaf.nodes(tokens.last) = new Node(0)
          case x =>
            val leaf = cwd.drop(1).foldLeft(tree.head) { (node, p) => {
              node.size += x.toInt
              node.nodes(p)
            }
            }
            leaf.size += x.toInt
        }
      }

      // tree.printTree()

      val sum = tree.filterTreeValues(_ <= 100000).sum
      println(s"Day 07, 1st part: $sum")

      val unused = 70_000_000 - tree.head.size
      val needToFree = 30_000_000 - unused
      val min = tree.filterTreeValues(_ >= needToFree).min
      println(s"Day 07, 2nd part: $min")
    }
  }
}

object Day7 {
  def apply(): Try[Unit] = {
    new Day7(7).execute()
  }
}
