object HamiltonianCircuit {

  // Function to check if vertex `v` can be added at index `pos` in the Hamiltonian cycle
  def isSafe(v: Int, graph: Array[Array[Int]], path: Array[Int], pos: Int): Boolean = {
    // Check if this vertex is an adjacent vertex of the previously added vertex
    if (graph(path(pos - 1))(v) == 0)
      return false

    // Check if the vertex has already been included
    for (i <- 0 until pos) {
      if (path(i) == v)
        return false
    }

    true
  }

  // Function to solve the Hamiltonian Circuit problem using backtracking
  def hamiltonianCircuitUtil(graph: Array[Array[Int]], path: Array[Int], pos: Int): Boolean = {
    val V = graph.length

    // Base case: if all vertices are included in the Hamiltonian path
    if (pos == V) {
      // And if there is an edge from the last included vertex to the first vertex
      if (graph(path(pos - 1))(path(0)) == 1)
        return true
      else
        return false
    }

    // Try different vertices as the next candidate in the Hamiltonian Circuit
    for (v <- 1 until V) {
      if (isSafe(v, graph, path, pos)) {
        path(pos) = v

        if (hamiltonianCircuitUtil(graph, path, pos + 1))
          return true

        // If adding vertex `v` doesn't lead to a solution, remove it from the path
        path(pos) = -1
      }
    }

    false
  }

  // Function to solve the Hamiltonian Circuit problem using the adjacency matrix representation of the graph
  def hamiltonianCircuit(graph: Array[Array[Int]]): Unit = {
    val V = graph.length
    val path = new Array[Int](V)
    for (i <- 0 until V)
      path(i) = -1

    // Start from the first vertex. We can choose any vertex as the starting vertex
    path(0) = 0

    if (hamiltonianCircuitUtil(graph, path, 1)) {
      println("Hamiltonian Circuit found:")
      for (i <- 0 until V)
        print(path(i) + " ")
      println(path(0)) // Print the first vertex again to show the complete cycle
    } else {
      println("No Hamiltonian Circuit exists for the given graph.")
    }
  }

  def main(args: Array[String]): Unit = {
    // Example usage:
    val graph = Array(
      Array(0, 1, 0, 1, 0),
      Array(1, 0, 1, 1, 1),
      Array(0, 1, 0, 0, 1),
      Array(1, 1, 0, 0, 1),
      Array(0, 1, 1, 1, 0)
    )

    hamiltonianCircuit(graph)
  }
}
