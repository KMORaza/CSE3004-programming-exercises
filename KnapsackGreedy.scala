object KnapsackGreedy {

  case class Item(weight: Int, value: Int)

  def knapsackGreedy(weights: Array[Int], values: Array[Int], W: Int): Int = {
    require(weights.length == values.length)

    val items = weights.zip(values).map { case (w, v) => Item(w, v) }
    val sortedItems = items.sortBy(item => -item.value.toDouble / item.weight)

    var currentWeight = 0
    var totalValue = 0

    for (item <- sortedItems) {
      if (currentWeight + item.weight <= W) {
        currentWeight += item.weight
        totalValue += item.value
      } else {
        val remainingCapacity = W - currentWeight
        totalValue += (item.value.toDouble / item.weight * remainingCapacity).toInt
        currentWeight = W
        // break since knapsack is full
      }
    }

    totalValue
  }

  def main(args: Array[String]): Unit = {
    val weights = Array(10, 20, 30)
    val values = Array(60, 100, 120)
    val W = 50

    val maxValue = knapsackGreedy(weights, values, W)
    println(s"Maximum value using Greedy approach: $maxValue")
  }
}
