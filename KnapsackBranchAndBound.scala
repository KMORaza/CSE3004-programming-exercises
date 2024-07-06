object KnapsackBranchAndBound {

  case class Item(weight: Int, value: Int)

  def knapsackBranchAndBound(weights: Array[Int], values: Array[Int], W: Int): Int = {
    require(weights.length == values.length)

    val n = weights.length
    val items = weights.zip(values).map { case (w, v) => Item(w, v) }
    val sortedItems = items.sortBy(item => -item.value.toDouble / item.weight)

    def bound(level: Int, weight: Int, value: Int): Int = {
      var boundValue = value
      var currentWeight = weight
      var i = level
      while (i < n && currentWeight + sortedItems(i).weight <= W) {
        currentWeight += sortedItems(i).weight
        boundValue += sortedItems(i).value
        i += 1
      }
      if (i < n) {
        boundValue += (W - currentWeight) * sortedItems(i).value / sortedItems(i).weight
      }
      boundValue
    }

    def branchAndBound(level: Int, weight: Int, value: Int): Int = {
      if (weight > W) {
        0
      } else if (level >= n) {
        value
      } else {
        val include = branchAndBound(level + 1, weight + sortedItems(level).weight, value + sortedItems(level).value)
        val exclude = branchAndBound(level + 1, weight, value)
        math.max(include, exclude)
      }
    }

    branchAndBound(0, 0, 0)
  }

  def main(args: Array[String]): Unit = {
    val weights = Array(10, 20, 30)
    val values = Array(60, 100, 120)
    val W = 50

    val maxValue = knapsackBranchAndBound(weights, values, W)
    println(s"Maximum value using Branch and Bound approach: $maxValue")
  }
}
