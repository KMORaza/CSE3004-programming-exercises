import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object PrimAlgorithm {

  type Graph = Array[Array[(Int, Int)]]

  def prim(graph: Graph): ArrayBuffer[(Int, Int)] = {
    val numVertices = graph.length
    val mst = new ArrayBuffer[(Int, Int)]()
    val visited = Array.fill[Boolean](numVertices)(false)
    val pq = mutable.PriorityQueue.empty(Ordering.by[(Int, Int), Int](-_._2))

    def visit(vertex: Int): Unit = {
      visited(vertex) = true
      for ((neighbor, weight) <- graph(vertex)) {
        if (!visited(neighbor)) {
          pq.enqueue((neighbor, weight))
        }
      }
    }

    visit(0) // Start from vertex 0

    while (pq.nonEmpty) {
      val (vertex, weight) = pq.dequeue()
      if (!visited(vertex)) {
        mst += ((vertex, weight))
        visit(vertex)
      }
    }

    mst
  }

  def main(args: Array[String]): Unit = {
    // Example graph represented as an adjacency list
    val graph: Graph = Array(
      Array((1, 2), (2, 3)),
      Array((0, 2), (2, 4)),
      Array((0, 3), (1, 4))
    )

    val mst = prim(graph)
    println("Edges in MST by Prim's algorithm:")
    mst.foreach { case (v1, v2) =>
      println(s"$v1 -- ${graph(v1).find(_._1 == v2).get._2} -- $v2")
    }
  }
}
