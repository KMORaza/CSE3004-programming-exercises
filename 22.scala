import scala.collection.mutable

object MaximumMatchingBipartiteGraph {

  // Function to find maximum matching in a bipartite graph
  def maximumMatching(graph: Array[Array[Boolean]], m: Int, n: Int): Array[Int] = {
    // Array to store matching of n vertices, -1 indicates no match
    val matchR = Array.fill(n)(-1)

    // Try to find an augmenting path
    def bpm(u: Int, seen: Array[Boolean], matchR: Array[Int]): Boolean = {
      for (v <- 0 until n) {
        if (graph(u)(v) && !seen(v)) {
          seen(v) = true
          if (matchR(v) == -1 || bpm(matchR(v), seen, matchR)) {
            matchR(v) = u
            return true
          }
        }
      }
      false
    }

    // Count of maximum matching
    var result = 0

    // Iterate through all vertices on left side
    for (u <- 0 until m) {
      // Array to track which vertices are visited in this DFS
      val seen = Array.fill(n)(false)

      // Find if the vertex u can be matched to a vertex on right side
      if (bpm(u, seen, matchR))
        result += 1
    }

    matchR
  }

  def main(args: Array[String]): Unit = {
    // Example usage:
    val m = 4 // Number of vertices in set U
    val n = 4 // Number of vertices in set V

    // Example adjacency matrix representation of the bipartite graph
    val graph = Array(
      Array(false, true, true, false),
      Array(true, false, false, true),
      Array(false, false, true, false),
      Array(false, false, true, false)
    )

    val matchR = maximumMatching(graph, m, n)

    // Print the maximum matching
    println("Maximum Matching:")
    for (i <- 0 until n) {
      if (matchR(i) != -1) {
        println(s"Vertex $matchR(i) matches with vertex $i")
      }
    }
  }
}
