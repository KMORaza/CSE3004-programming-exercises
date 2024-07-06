object KnapsackBacktracking {

  def knapsackBacktracking(weights: Array[Int], values: Array[Int], W: Int): Int = {
    require(weights.length == values.length)

    val n = weights.length

    def backtrack(index: Int, currentWeight: Int, currentValue: Int): Int = {
      if (index >= n || currentWeight == W) {
        currentValue
      } else {
        // Include current item if it fits
        val valueInclude = if (currentWeight + weights(index) <= W) {
          backtrack(index + 1, currentWeight + weights(index), currentValue + values(index))
        } else {
          currentValue
        }

        // Exclude current item
        val valueExclude = backtrack(index + 1, currentWeight, currentValue)

        math.max(valueInclude, valueExclude)
      }
    }

    backtrack(0, 0, 0)
  }

  def main(args: Array[String]): Unit = {
    val weights = Array(10, 20, 30)
    val values = Array(60, 100, 120)
    val W = 50

    val maxValue = knapsackBacktracking(weights, values, W)
    println(s"Maximum value using Backtracking approach: $maxValue")
  }
}
