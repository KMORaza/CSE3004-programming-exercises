object BranchAndBoundStringMatching {

  // Function to find all occurrences of pattern in text using Branch and Bound
  def findAllOccurrences(text: String, pattern: String): List[Int] = {
    val n = text.length
    val m = pattern.length

    // Function to check if pattern can occur starting at position `pos` in text
    def canOccur(pos: Int): Boolean = {
      var i = 0
      while (i < m && text(pos + i) == pattern(i)) {
        i += 1
      }
      i == m // True if we matched entire pattern
    }

    // List to store all occurrences of pattern
    var occurrences: List[Int] = List()

    // Recursive function to perform Branch and Bound
    def search(pos: Int): Unit = {
      if (pos <= n - m) {
        if (canOccur(pos)) {
          occurrences = pos :: occurrences
          search(pos + 1)
        } else {
          search(pos + 1)
        }
      }
    }

    // Start the search from position 0 in text
    search(0)

    // Return the list of occurrences found
    occurrences.reverse
  }

  def main(args: Array[String]): Unit = {
    val text = "ababcababcabcabc"
    val pattern = "abc"

    val occurrences = findAllOccurrences(text, pattern)
    println(s"Occurrences of pattern '$pattern' in text '$text' are at positions: $occurrences")
  }
}
