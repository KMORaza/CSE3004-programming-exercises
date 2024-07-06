object KnapsackDP {

  def knapsackDP(weights: Array[Int], values: Array[Int], W: Int): Int = {
    require(weights.length == values.length)

    val n = weights.length
    val dp = Array.ofDim[Int](n + 1, W + 1)

    for {
      i <- 1 to n
      w <- 1 to W
    } {
      if (weights(i - 1) <= w) {
        dp(i)(w) = math.max(dp(i - 1)(w), dp(i - 1)(w - weights(i - 1)) + values(i - 1))
      } else {
        dp(i)(w) = dp(i - 1)(w)
      }
    }

    dp(n)(W)
  }

  def main(args: Array[String]): Unit = {
    val weights = Array(10, 20, 30)
    val values = Array(60, 100, 120)
    val W = 50

    val maxValue = knapsackDP(weights, values, W)
    println(s"Maximum value using Dynamic Programming approach: $maxValue")
  }
}
