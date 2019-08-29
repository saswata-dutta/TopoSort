import scala.collection.mutable

object TopoOrder {

  def apply[T](edges: Seq[Edge[T]]): Either[Set[T], Seq[T]] = {
    var (neighbors, inDegrees, nodes) = makeGraph(edges)
    val sources = mutable.Queue[T](independentSources(inDegrees): _*)

    val topoOrder = mutable.Buffer[T]()
    while (sources.nonEmpty) {
      val source = sources.dequeue
      topoOrder += source

      val (newInDegrees, newSources) =
        decrementInDegrees(inDegrees, neighbors.getOrElse(source, Set.empty[T]))

      inDegrees = newInDegrees
      sources ++= newSources
    }
    if (topoOrder.length < nodes.size)
      Left(nodes -- topoOrder.toSet)
    else Right(topoOrder)
  }

  def makeGraph[T](edges: Seq[Edge[T]]): (Map[T, Set[T]], Map[T, Int], Set[T]) =
    edges.foldLeft((Map.empty[T, Set[T]], Map.empty[T, Int], Set.empty[T])) {
      case ((neighbors, inDegrees, nodes), edge) =>
        val inDegreeDst = inDegrees.getOrElse(edge.dst, 0) + 1
        val inDegreeSrc = inDegrees.getOrElse(edge.src, 0)
        val inDegrees1 = inDegrees + (edge.dst -> inDegreeDst) + (edge.src -> inDegreeSrc)

        val newNeighbors = neighbors.getOrElse(edge.src, Set.empty[T]) + edge.dst
        val neighbors1 = neighbors + (edge.src -> newNeighbors)
        (neighbors1, inDegrees1, nodes ++ Set(edge.src, edge.dst))
    }

  def independentSources[T](inDegrees: Map[T, Int]): Seq[T] =
    inDegrees.filter { case (_, v) => v < 1 }.keys.toSeq

  def decrementInDegrees[T](inDegrees: Map[T, Int], dependants: Set[T]): (Map[T, Int], Set[T]) =
    dependants.foldLeft(inDegrees, Set.empty[T]) {
      case ((inDegs, newSources), dep) =>
        val deg = inDegs.getOrElse(dep, 0)
        if (deg == 1) {
          (inDegs + (dep -> 0), newSources + dep)
        } else (inDegs + (dep -> (deg - 1)), newSources)
    }
}
