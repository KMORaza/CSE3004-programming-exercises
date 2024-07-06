object SubsetSum {

  def subsetSumExists(nums: Array[Int], target: Int): Boolean = {
    val n = nums.length
    val dp = Array.ofDim[Boolean](n + 1, target + 1)

    // Base case initialization
    for (i <- 0 to n) {
      dp(i)(0) = true // Zero sum is always possible with an empty subset
    }

    // Fill the dp table
    for {
      i <- 1 to n
      currentNum = nums(i - 1)
      j <- 1 to target
    } {
      dp(i)(j) = dp(i - 1)(j) || (j >= currentNum && dp(i - 1)(j - currentNum))
    }

    // The answer is found in dp(n)(target)
    dp(n)(target)
  }

  def main(args: Array[String]): Unit = {
    val nums = Array(3, 34, 4, 12, 5, 2)
    val target = 9
    if (subsetSumExists(nums, target)) {
      println(s"There exists a subset that sums up to $target")
    } else {
      println(s"No subset sums up to $target")
    }
  }
}
