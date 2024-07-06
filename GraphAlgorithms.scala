object GraphAlgorithms {

  // Warshall's algorithm for computing transitive closure
  def warshallClosure(graph: Array[Array[Boolean]]): Array[Array[Boolean]] = {
    val n = graph.length
    val closure = Array.ofDim[Boolean](n, n)

    // Initialize closure with the given graph
    for (i <- 0 until n; j <- 0 until n) {
      closure(i)(j) = graph(i)(j)
    }

    // Warshall's algorithm
    for (k <- 0 until n; i <- 0 until n; j <- 0 until n) {
      closure(i)(j) = closure(i)(j) || (closure(i)(k) && closure(k)(j))
    }

    closure
  }

  // Floyd's algorithm for computing shortest paths
  def floydShortestPaths(graph: Array[Array[Int]]): Array[Array[Int]] = {
    val n = graph.length
    val dist = Array.ofDim[Int](n, n)

    // Initialize dist with the given graph
    for (i <- 0 until n; j <- 0 until n) {
      dist(i)(j) = graph(i)(j)
    }

    // Floyd's algorithm
    for (k <- 0 until n; i <- 0 until n; j <- 0 until n) {
      if (dist(i)(j) > dist(i)(k) + dist(k)(j)) {
        dist(i)(j) = dist(i)(k) + dist(k)(j)
      }
    }

    dist
  }

  def main(args: Array[String]): Unit = {
    // Example graph adjacency matrix for testing
    val graph = Array(
      Array(false, true, false, true),
      Array(false, false, true, false),
      Array(false, false, false, true),
      Array(false, false, false, false)
    )

    val adjMatrix = Array(
      Array(0, 5, Int.MaxValue, 10),
      Array(Int.MaxValue, 0, 3, Int.MaxValue),
      Array(Int.MaxValue, Int.MaxValue, 0, 1),
      Array(Int.MaxValue, Int.MaxValue, Int.MaxValue, 0)
    )

    // Compute transitive closure using Warshall's algorithm
    val warshallResult = warshallClosure(graph)
    println("Warshall's Transitive Closure:")
    warshallResult.foreach(row => println(row.mkString("\t")))

    // Compute shortest paths using Floyd's algorithm
    val floydResult = floydShortestPaths(adjMatrix)
    println("\nFloyd's Shortest Paths:")
    floydResult.foreach(row => println(row.map(value => if (value == Int.MaxValue) "∞" else value.toString).mkString("\t")))
  }
}
