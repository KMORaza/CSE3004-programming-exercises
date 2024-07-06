import scala.collection.mutable.ArrayBuffer

object GraphColoring {

  // Function to check if it's safe to color vertex v with color c
  def isSafe(v: Int, graph: Array[Array[Int]], color: Array[Int], c: Int): Boolean = {
    for (i <- 0 until graph.length) {
      if (graph(v)(i) == 1 && c == color(i)) {
        return false
      }
    }
    true
  }

  // Function to recursively solve the graph coloring problem
  def graphColoringUtil(graph: Array[Array[Int]], m: Int, color: Array[Int], v: Int): Boolean = {
    // If all vertices are colored
    if (v == graph.length) {
      return true
    }

    // Try different colors for vertex v
    for (c <- 1 to m) {
      // Check if assignment of color c to v is possible
      if (isSafe(v, graph, color, c)) {
        color(v) = c

        // Recur to assign colors to rest of the vertices
        if (graphColoringUtil(graph, m, color, v + 1)) {
          return true
        }

        // If assigning color c doesn't lead to a solution, backtrack
        color(v) = 0
      }
    }

    false
  }

  // Function to solve the graph coloring problem using backtracking
  def graphColoring(graph: Array[Array[Int]], m: Int): Unit = {
    val V = graph.length
    // Initialize all vertices as unassigned (0)
    val color = Array.fill(V)(0)

    // Call graphColoringUtil to solve the problem
    if (!graphColoringUtil(graph, m, color, 0)) {
      println("Solution does not exist")
    } else {
      // Print the solution
      println("Solution found:")
      for (i <- 0 until V) {
        println(s"Vertex $i --> Color ${color(i)}")
      }
    }
  }

  def main(args: Array[String]): Unit = {
    // Example graph represented as adjacency matrix
    val graph = Array(
      Array(0, 1, 1, 1),
      Array(1, 0, 1, 0),
      Array(1, 1, 0, 1),
      Array(1, 0, 1, 0)
    )

    val m = 3 // Number of colors

    graphColoring(graph, m)
  }
}
