import scala.collection.mutable

object AssignmentProblemSolver {

  // Implementation of the Hungarian algorithm
  def hungarianAlgorithm(costMatrix: Array[Array[Int]]): Array[(Int, Int)] = {
    val numRows = costMatrix.length
    val numCols = costMatrix(0).length

    // Step 1: Subtract the minimum value of each row from all elements of that row
    val reducedMatrix = Array.ofDim[Int](numRows, numCols)
    for (i <- 0 until numRows) {
      val minRowValue = costMatrix(i).min
      for (j <- 0 until numCols) {
        reducedMatrix(i)(j) = costMatrix(i)(j) - minRowValue
      }
    }

    // Step 2: Subtract the minimum value of each column from all elements of that column
    for (j <- 0 until numCols) {
      val minColValue = (for (i <- 0 until numRows) yield reducedMatrix(i)(j)).min
      for (i <- 0 until numRows) {
        reducedMatrix(i)(j) -= minColValue
      }
    }

    // Step 3: Initialize variables
    val rowCovered = Array.fill[Boolean](numRows)(false)
    val colCovered = Array.fill[Boolean](numCols)(false)
    val assignment = mutable.Map[Int, Int]() // Map from column to row

    // Step 4: Try to find a valid assignment
    var numAssigned = 0
    while (numAssigned < numRows) {
      val (row, col) = findUncoveredZero(reducedMatrix, rowCovered, colCovered)
      if (row == -1) {
        // If no uncovered zero found, perform step 6
        val (minUncoveredValue, (uncoveredRow, uncoveredCol)) = findMinUncoveredValue(reducedMatrix, rowCovered, colCovered)
        updateMatrix(reducedMatrix, rowCovered, colCovered, minUncoveredValue)
      } else {
        // If uncovered zero found, mark row and column as covered
        rowCovered(row) = true
        colCovered(col) = true
        assignment(col) = row
        numAssigned += 1
      }
    }

    // Convert assignment map to array of (row, col) pairs
    assignment.toArray.sortBy(_._1)
  }

  // Helper function to find an uncovered zero and return its coordinates
  private def findUncoveredZero(matrix: Array[Array[Int]], rowCovered: Array[Boolean], colCovered: Array[Boolean]): (Int, Int) = {
    val numRows = matrix.length
    val numCols = matrix(0).length

    for (i <- 0 until numRows) {
      for (j <- 0 until numCols) {
        if (matrix(i)(j) == 0 && !rowCovered(i) && !colCovered(j)) {
          return (i, j)
        }
      }
    }
    (-1, -1)
  }

  // Helper function to find the minimum uncovered value in the reduced matrix
  private def findMinUncoveredValue(matrix: Array[Array[Int]], rowCovered: Array[Boolean], colCovered: Array[Boolean]): (Int, (Int, Int)) = {
    val numRows = matrix.length
    val numCols = matrix(0).length
    var minUncoveredValue = Int.MaxValue
    var minCoordinates = (-1, -1)

    for (i <- 0 until numRows) {
      for (j <- 0 until numCols) {
        if (!rowCovered(i) && !colCovered(j) && matrix(i)(j) < minUncoveredValue) {
          minUncoveredValue = matrix(i)(j)
          minCoordinates = (i, j)
        }
      }
    }
    (minUncoveredValue, minCoordinates)
  }

  // Helper function to update the reduced matrix after step 6
  private def updateMatrix(matrix: Array[Array[Int]], rowCovered: Array[Boolean], colCovered: Array[Boolean], minUncoveredValue: Int): Unit = {
    val numRows = matrix.length
    val numCols = matrix(0).length

    for (i <- 0 until numRows) {
      for (j <- 0 until numCols) {
        if (rowCovered(i)) {
          matrix(i)(j) += minUncoveredValue
        }
        if (!colCovered(j)) {
          matrix(i)(j) -= minUncoveredValue
        }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    // Example cost matrix
    val costMatrix = Array(
      Array(3, 2, 7),
      Array(2, 4, 6),
      Array(5, 8, 1)
    )

    // Solve the assignment problem
    val assignment = hungarianAlgorithm(costMatrix)

    // Print the result
    println("Optimal Assignment:")
    assignment.foreach { case (col, row) =>
      println(s"Assign task $col to agent $row")
    }
  }
}
