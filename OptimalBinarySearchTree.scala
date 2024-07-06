object OptimalBinarySearchTree {

  def optimalSearchTree(keys: Array[Int], freq: Array[Int]): Int = {
    val n = keys.length
    val cost = Array.ofDim[Int](n, n)

    // Cost is zero when there is only one element in the subarray
    for (i <- 0 until n)
      cost(i)(i) = freq(i)

    // Build the table `cost` in bottom up manner
    for (length <- 2 to n) {
      for (i <- 0 to n - length) {
        val j = i + length - 1
        cost(i)(j) = Int.MaxValue
        var sum = sumFreq(freq, i, j)

        // Try making all keys in interval [i, j] as root
        for (r <- i to j) {
          val c = sum + (if (r - 1 < i) 0 else cost(i)(r - 1)) +
            (if (r + 1 > j) 0 else cost(r + 1)(j))
          if (c < cost(i)(j))
            cost(i)(j) = c
        }
      }
    }

    // Return the minimum cost to construct BST from keys[0] to keys[n-1]
    cost(0)(n - 1)
  }

  def sumFreq(freq: Array[Int], i: Int, j: Int): Int = {
    var sum = 0
    for (k <- i to j)
      sum += freq(k)
    sum
  }

  def main(args: Array[String]): Unit = {
    val keys = Array(10, 12, 20, 35, 46)
    val freq = Array(34, 8, 50, 21, 16)

    val minCost = optimalSearchTree(keys, freq)
    println(s"Minimum cost to construct optimal BST is: $minCost")
  }
}
