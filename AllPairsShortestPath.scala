object AllPairsShortestPath {

  // Function to perform All-Pairs Shortest Path using Floyd-Warshall algorithm
  def floydWarshall(graph: Array[Array[Int]], V: Int): Array[Array[Int]] = {
    // Initialize distances array with the given graph
    val dist = Array.ofDim[Int](V, V)
    for (i <- 0 until V; j <- 0 until V) {
      dist(i)(j) = graph(i)(j)
    }

    // Apply Floyd-Warshall algorithm
    for (k <- 0 until V) {
      for (i <- 0 until V) {
        for (j <- 0 until V) {
          if (dist(i)(k) != Int.MaxValue && dist(k)(j) != Int.MaxValue &&
              dist(i)(j) > dist(i)(k) + dist(k)(j)) {
            dist(i)(j) = dist(i)(k) + dist(k)(j)
          }
        }
      }
    }

    // Return the computed shortest distances
    dist
  }

  def main(args: Array[String]): Unit = {
    // Example graph represented as adjacency matrix
    val graph = Array(
      Array(0, 5, Int.MaxValue, 10),
      Array(Int.MaxValue, 0, 3, Int.MaxValue),
      Array(Int.MaxValue, Int.MaxValue, 0, 1),
      Array(Int.MaxValue, Int.MaxValue, Int.MaxValue, 0)
    )

    val V = graph.length

    // Compute All-Pairs Shortest Path
    val shortestDistances = floydWarshall(graph, V)

    // Output the shortest distances
    println("Shortest distances between every pair of vertices:")
    for (i <- 0 until V) {
      for (j <- 0 until V) {
        if (shortestDistances(i)(j) == Int.MaxValue) {
          print("INF ")
        } else {
          print(shortestDistances(i)(j) + " ")
        }
      }
      println()
    }
  }
}
