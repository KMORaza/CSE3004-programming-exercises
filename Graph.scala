import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class Graph(val vertices: Int) {
  val adjacencyList: Array[ListBuffer[Int]] = Array.fill(vertices)(ListBuffer())

  def addEdge(from: Int, to: Int): Unit = {
    adjacencyList(from).append(to)
  }

  def transpose(): Graph = {
    val transposedGraph = new Graph(vertices)
    for (v <- 0 until vertices) {
      for (adj <- adjacencyList(v)) {
        transposedGraph.addEdge(adj, v)
      }
    }
    transposedGraph
  }

  def dfsUtil(vertex: Int, visited: Array[Boolean], stack: mutable.Stack[Int]): Unit = {
    visited(vertex) = true
    for (adj <- adjacencyList(vertex)) {
      if (!visited(adj)) {
        dfsUtil(adj, visited, stack)
      }
    }
    stack.push(vertex)
  }

  def dfs(vertex: Int, visited: Array[Boolean], component: ListBuffer[Int]): Unit = {
    visited(vertex) = true
    component.append(vertex)
    for (adj <- adjacencyList(vertex)) {
      if (!visited(adj)) {
        dfs(adj, visited, component)
      }
    }
  }

  def getSCCs(): List[List[Int]] = {
    val stack = new mutable.Stack[Int]()
    val visited = Array.fill(vertices)(false)

    // Step 1: Fill vertices in stack according to their finishing times
    for (v <- 0 until vertices) {
      if (!visited(v)) {
        dfsUtil(v, visited, stack)
      }
    }

    // Step 2: Create the transpose of the graph
    val transposedGraph = transpose()

    // Step 3: Initialize visited array for the second DFS
    for (i <- visited.indices) {
      visited(i) = false
    }

    // Step 4: Process all vertices in order defined by stack
    var sccList = ListBuffer[List[Int]]()
    while (stack.nonEmpty) {
      val v = stack.pop()
      if (!visited(v)) {
        val component = ListBuffer[Int]()
        transposedGraph.dfs(v, visited, component)
        sccList.append(component.toList)
      }
    }

    sccList.toList
  }
}

// Example usage:
object Main {
  def main(args: Array[String]): Unit = {
    val graph = new Graph(5)
    graph.addEdge(1, 0)
    graph.addEdge(0, 2)
    graph.addEdge(2, 1)
    graph.addEdge(0, 3)
    graph.addEdge(3, 4)

    val sccs = graph.getSCCs()
    println("Strongly Connected Components:")
    for (scc <- sccs) {
      println(scc.mkString("(", ", ", ")"))
    }
  }
}
