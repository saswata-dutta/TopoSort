object Main {

  def main(args: Array[String]): Unit = {
    val edges =
      Seq(Edge(1, 2), Edge(1, 3), Edge(3, 2), Edge(4, 5), Edge(5, 6), Edge(2, 6), Edge(2, 4))
//    val edges = readDependencies()
    val topoOrder = TopoOrder(edges)
    println(topoOrder)
  }

  def readDependencies(): Seq[Edge[Int]] = {
    val numEdges = scala.io.StdIn.readInt()

    (1 to numEdges).map { _ =>
      val input = scala.io.StdIn.readLine()
      val Array(child, parent) = input.split("\\s+").filter(_.nonEmpty).map(_.toInt)
      Edge(parent, child)
    }
  }
}
