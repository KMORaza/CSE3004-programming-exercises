object MatrixChainMultiplication {

  // Function to find the optimal order of matrix multiplication
  def findOptimalOrder(dimensions: Array[Int]): (Int, Array[Array[Int]]) = {
    val n = dimensions.length - 1 // number of matrices

    // Create a 2D array to store minimum number of scalar multiplications
    val dp = Array.ofDim[Int](n, n)
    // Create a 2D array to store optimal split points
    val split = Array.ofDim[Int](n, n)

    // Initialize the dp array with zeros (though Java/Scala initializes with 0 automatically)
    // dp[i][j] = Minimum number of scalar multiplications needed to multiply matrices from i to j

    // For single matrix, cost is 0 (no multiplication needed)
    for (i <- 0 until n) {
      dp(i)(i) = 0
    }

    // l is the chain length
    for (l <- 2 to n) {
      for (i <- 0 until n - l + 1) {
        val j = i + l - 1
        dp(i)(j) = Int.MaxValue
        for (k <- i until j) {
          val q = dp(i)(k) + dp(k + 1)(j) + dimensions(i) * dimensions(k + 1) * dimensions(j + 1)
          if (q < dp(i)(j)) {
            dp(i)(j) = q
            split(i)(j) = k
          }
        }
      }
    }

    // Return the minimum number of scalar multiplications and the split array
    (dp(0)(n - 1), split)
  }

  // Function to print the optimal order of multiplication
  def printOptimalOrder(split: Array[Array[Int]], i: Int, j: Int): String = {
    if (i == j) {
      s"M$i"
    } else {
      val k = split(i)(j)
      "(" + printOptimalOrder(split, i, k) + " × " + printOptimalOrder(split, k + 1, j) + ")"
    }
  }

  def main(args: Array[String]): Unit = {
    val dimensions = Array(10, 30, 5, 60) // Example dimensions of matrices
    val (minCost, split) = findOptimalOrder(dimensions)

    println(s"Minimum number of scalar multiplications required: $minCost")
    println("Optimal order of multiplication: " + printOptimalOrder(split, 0, dimensions.length - 2))
  }
}
