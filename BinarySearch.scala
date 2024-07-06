object BinarySearch {
  def binarySearch(arr: Array[Int], target: Int): Option[Int] = {
    def search(low: Int, high: Int): Option[Int] = {
      if (low > high) {
        None
      } else {
        val mid = low + (high - low) / 2
        arr(mid) match {
          case `target` => Some(mid)
          case x if x < target => search(mid + 1, high)
          case _ => search(low, mid - 1)
        }
      }
    }

    search(0, arr.length - 1)
  }

  def main(args: Array[String]): Unit = {
    val arr = Array(3, 9, 10, 27, 38, 43, 82)
    val target = 38
    val result = binarySearch(arr, target)
    result match {
      case Some(index) => println(s"Target $target found at index $index")
      case None => println(s"Target $target not found in the array")
    }
  }
}
