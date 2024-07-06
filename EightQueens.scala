object EightQueens {

  def main(args: Array[String]): Unit = {
    solveQueens(8)
  }

  def solveQueens(n: Int): Unit = {
    val board = Array.ofDim[Boolean](n, n)
    if (placeQueens(board, 0, n)) {
      println(s"Solution found for ${n} queens:")
      printBoard(board)
    } else {
      println(s"No solution found for ${n} queens.")
    }
  }

  def placeQueens(board: Array[Array[Boolean]], col: Int, n: Int): Boolean = {
    if (col >= n) {
      true
    } else {
      var placed = false
      var row = 0
      while (row < n && !placed) {
        if (isSafe(board, row, col, n)) {
          board(row)(col) = true
          placed = placeQueens(board, col + 1, n)
          if (!placed) {
            board(row)(col) = false // backtrack
          }
        }
        row += 1
      }
      placed
    }
  }

  def isSafe(board: Array[Array[Boolean]], row: Int, col: Int, n: Int): Boolean = {
    // Check if no two queens share the same column
    for (i <- 0 until col) {
      if (board(row)(i)) {
        return false
      }
    }

    // Check upper diagonal on left side
    var i = row
    var j = col
    while (i >= 0 && j >= 0) {
      if (board(i)(j)) {
        return false
      }
      i -= 1
      j -= 1
    }

    // Check lower diagonal on left side
    i = row
    j = col
    while (i < n && j >= 0) {
      if (board(i)(j)) {
        return false
      }
      i += 1
      j -= 1
    }

    true
  }

  def printBoard(board: Array[Array[Boolean]]): Unit = {
    val n = board.length
    for (i <- 0 until n) {
      for (j <- 0 until n) {
        if (board(i)(j)) print("Q ") else print(". ")
      }
      println()
    }
  }

}
