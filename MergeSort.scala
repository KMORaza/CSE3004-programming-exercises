object MergeSort {
  def mergeSort(arr: Array[Int]): Array[Int] = {
    def merge(left: Array[Int], right: Array[Int]): Array[Int] = {
      var result = Array[Int]()
      var i = 0
      var j = 0
      while (i < left.length && j < right.length) {
        if (left(i) <= right(j)) {
          result :+= left(i)
          i += 1
        } else {
          result :+= right(j)
          j += 1
        }
      }
      if (i < left.length) {
        result ++= left.slice(i, left.length)
      }
      if (j < right.length) {
        result ++= right.slice(j, right.length)
      }
      result
    }

    if (arr.length <= 1) {
      arr
    } else {
      val mid = arr.length / 2
      val (left, right) = arr.splitAt(mid)
      merge(mergeSort(left), mergeSort(right))
    }
  }

  def main(args: Array[String]): Unit = {
    val arr = Array(38, 27, 43, 3, 9, 82, 10)
    val sortedArr = mergeSort(arr)
    println("Sorted array: " + sortedArr.mkString(", "))
  }
}
