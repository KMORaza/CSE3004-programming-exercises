import scala.collection.mutable.ArrayBuffer

object TSPDynamicProgramming {

  // Function to calculate the distance between two points
  def distance(point1: (Int, Int), point2: (Int, Int)): Double = {
    math.sqrt(math.pow(point1._1 - point2._1, 2) + math.pow(point1._2 - point2._2, 2))
  }

  // Function to solve TSP using Dynamic Programming
  def solveTSP(points: Array[(Int, Int)]): (Double, List[Int]) = {
    val n = points.length
    val dp = Array.ofDim[Double](1 << n, n)
    val path = Array.ofDim[Int](1 << n, n)

    // Initialize dp table with infinity
    for (i <- dp.indices; j <- 0 until n) {
      dp(i)(j) = Double.PositiveInfinity
    }

    // Initialize base case
    dp(1)(0) = 0

    // Fill the dp table
    for (mask <- 1 until (1 << n); last <- 0 until n) {
      if ((mask & (1 << last)) != 0) {
        for (current <- 0 until n) {
          if ((mask & (1 << current)) != 0 && current != last) {
            val newMask = mask ^ (1 << last)
            val newCost = dp(newMask)(current) + distance(points(current), points(last))
            if (newCost < dp(mask)(last)) {
              dp(mask)(last) = newCost
              path(mask)(last) = current
            }
          }
        }
      }
    }

    // Find the minimum cost tour
    var minCost = Double.PositiveInfinity
    var lastNode = -1
    for (i <- 1 until n) {
      val cost = dp((1 << n) - 1)(i) + distance(points(i), points(0))
      if (cost < minCost) {
        minCost = cost
        lastNode = i
      }
    }

    // Reconstruct the path
    var mask = (1 << n) - 1
    val tour = ArrayBuffer[Int]()
    var current = lastNode
    while (current != 0) {
      tour.prepend(current)
      val next = path(mask)(current)
      mask ^= (1 << current)
      current = next
    }
    tour.prepend(0)

    (minCost, tour.toList)
  }

  def main(args: Array[String]): Unit = {
    val points = Array((0, 0), (1, 2), (3, 1), (4, 3))
    val (cost, tour) = solveTSP(points)
    println(s"Minimum cost: $cost")
    println(s"Tour: ${tour.mkString(" -> ")}")
  }
}
