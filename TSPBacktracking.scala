import scala.collection.mutable.ArrayBuffer

object TSPBacktracking {

  // Function to calculate the distance between two points
  def distance(point1: (Int, Int), point2: (Int, Int)): Double = {
    math.sqrt(math.pow(point1._1 - point2._1, 2) + math.pow(point1._2 - point2._2, 2))
  }

  // Function to solve TSP using Backtracking
  def solveTSP(points: Array[(Int, Int)]): (Double, List[Int]) = {
    var bestCost = Double.PositiveInfinity
    var bestTour = List[Int]()

    def backtrack(path: ArrayBuffer[Int], visited: Array[Boolean], currentCost: Double): Unit = {
      if (path.length == points.length) {
        val cost = currentCost + distance(points(path.last), points(path.head))
        if (cost < bestCost) {
          bestCost = cost
          bestTour = path.toList
        }
      } else {
        for (i <- points.indices) {
          if (!visited(i)) {
            val newPath = path.clone()
            newPath.append(i)
            val newVisited = visited.clone()
            newVisited(i) = true
            val newCost = if (path.isEmpty) 0 else currentCost + distance(points(path.last), points(i))
            if (newCost < bestCost) {
              backtrack(newPath, newVisited, newCost)
            }
          }
        }
      }
    }

    backtrack(ArrayBuffer[Int](), Array.fill(points.length)(false), 0)

    (bestCost, bestTour)
  }

  def main(args: Array[String]): Unit = {
    val points = Array((0, 0), (1, 2), (3, 1), (4, 3))
    val (cost, tour) = solveTSP(points)
    println(s"Minimum cost: $cost")
    println(s"Tour: ${tour.map(i => points(i)).mkString(" -> ")}")
  }
}
