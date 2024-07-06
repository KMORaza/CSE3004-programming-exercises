import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object KruskalAlgorithm {

  type Edge = (Int, Int, Int) // (vertex1, vertex2, weight)
  type Graph = List[Edge]

  def kruskal(graph: Graph, numVertices: Int): ArrayBuffer[Edge] = {
    val mst = new ArrayBuffer[Edge]()
    val dsu = new DisjointSetUnion(numVertices)

    // Sort edges by weight
    val sortedEdges = graph.sortBy(_._3)

    for ((v1, v2, weight) <- sortedEdges) {
      if (!dsu.connected(v1, v2)) {
        dsu.union(v1, v2)
        mst += ((v1, v2, weight))
      }
    }

    mst
  }

  class DisjointSetUnion(size: Int) {
    private val parent = Array.tabulate[Int](size)(identity)
    private val rank = Array.fill[Int](size)(0)

    def find(x: Int): Int = {
      if (parent(x) != x) {
        parent(x) = find(parent(x))
      }
      parent(x)
    }

    def union(x: Int, y: Int): Unit = {
      val rootX = find(x)
      val rootY = find(y)
      if (rootX != rootY) {
        if (rank(rootX) > rank(rootY)) {
          parent(rootY) = rootX
        } else if (rank(rootX) < rank(rootY)) {
          parent(rootX) = rootY
        } else {
          parent(rootY) = rootX
          rank(rootX) += 1
        }
      }
    }

    def connected(x: Int, y: Int): Boolean = {
      find(x) == find(y)
    }
  }

  def main(args: Array[String]): Unit = {
    // Example graph represented as a list of edges (vertex1, vertex2, weight)
    val graph: Graph = List(
      (0, 1, 2),
      (0, 2, 3),
      (1, 2, 4)
    )

    val numVertices = 3
    val mst = kruskal(graph, numVertices)
    println("Edges in MST by Kruskal's algorithm:")
    mst.foreach { case (v1, v2, weight) =>
      println(s"$v1 -- $weight -- $v2")
    }
  }
}
