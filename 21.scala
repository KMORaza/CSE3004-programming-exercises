import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Queue

object MaximumFlow {

  // Graph representation using adjacency list
  class FlowGraph(V: Int) {
    val adjList: Array[ArrayBuffer[FlowEdge]] = Array.fill(V)(new ArrayBuffer[FlowEdge]())

    def addEdge(from: Int, to: Int, capacity: Int): Unit = {
      val forwardEdge = FlowEdge(from, to, capacity)
      val backwardEdge = FlowEdge(to, from, 0) // Capacity of backward edge is 0 initially
      forwardEdge.reverseEdge = Some(backwardEdge)
      backwardEdge.reverseEdge = Some(forwardEdge)
      adjList(from).append(forwardEdge)
      adjList(to).append(backwardEdge)
    }

    def bfs(source: Int, sink: Int, parent: Array[Option[FlowEdge]]): Boolean = {
      val visited = Array.fill(V)(false)
      val queue = Queue[Int]()

      queue.enqueue(source)
      visited(source) = true

      while (queue.nonEmpty) {
        val current = queue.dequeue()

        for (edge <- adjList(current)) {
          if (!visited(edge.to) && edge.remainingCapacity() > 0) {
            parent(edge.to) = Some(edge)
            visited(edge.to) = true
            queue.enqueue(edge.to)

            if (edge.to == sink)
              return true
          }
        }
      }

      false
    }

    def edmondsKarp(source: Int, sink: Int): Int = {
      val parent = Array.fill[Option[FlowEdge]](V)(None)
      var maxFlow = 0

      while (bfs(source, sink, parent)) {
        var pathFlow = Int.MaxValue
        var v = sink

        // Find the maximum flow through the path found by BFS
        while (v != source) {
          val edge = parent(v).get
          pathFlow = pathFlow min edge.remainingCapacity()
          v = edge.from
        }

        // Update capacities of the edges and reverse edges along the path
        v = sink
        while (v != source) {
          val edge = parent(v).get
          edge.addFlow(pathFlow)
          v = edge.from
        }

        maxFlow += pathFlow
      }

      maxFlow
    }
  }

  // Edge representation
  case class FlowEdge(from: Int, to: Int, var capacity: Int) {
    var reverseEdge: Option[FlowEdge] = None

    def remainingCapacity(): Int = capacity

    def addFlow(flow: Int): Unit = {
      capacity -= flow
      reverseEdge.foreach(_.capacity += flow)
    }
  }

  def main(args: Array[String]): Unit = {
    val V = 6 // Number of vertices
    val graph = new FlowGraph(V)

    // Adding edges to the graph
    graph.addEdge(0, 1, 16)
    graph.addEdge(0, 2, 13)
    graph.addEdge(1, 2, 10)
    graph.addEdge(1, 3, 12)
    graph.addEdge(2, 1, 4)
    graph.addEdge(2, 4, 14)
    graph.addEdge(3, 2, 9)
    graph.addEdge(3, 5, 20)
    graph.addEdge(4, 3, 7)
    graph.addEdge(4, 5, 4)

    val source = 0
    val sink = 5

    println(s"Maximum flow from source $source to sink $sink is: ${graph.edmondsKarp(source, sink)}")
  }
}
