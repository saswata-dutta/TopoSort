import TopoOrder.makeGraph
import org.scalatest.FlatSpec

class TopoOrderShould extends FlatSpec {
  "Topo order" should "make dependency graph from edges" in {
    val (neighbors, inDegrees, nodes) =
      makeGraph(Seq(Edge(1, 2), Edge(1, 3), Edge(2, 3)))

    assert(neighbors(1) == Set(2, 3))
    assert(neighbors(2) == Set(3))
    assert(neighbors.getOrElse(3, Set.empty[Int]) == Set())

    assert(inDegrees(3) == 2)
    assert(inDegrees(2) == 1)
    assert(inDegrees.getOrElse(1, 0) == 0)

    assert(nodes == Set(1, 2, 3))
  }

}
