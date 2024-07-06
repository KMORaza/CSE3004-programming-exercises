object QuickSort {
  def quickSort(arr: Array[Int]): Array[Int] = {
    if (arr.length <= 1) {
      arr
    } else {
      val pivot = arr(arr.length / 2)
      Array.concat(
        quickSort(arr.filter(_ < pivot)),
        arr.filter(_ == pivot),
        quickSort(arr.filter(_ > pivot))
      )
    }
  }

  def main(args: Array[String]): Unit = {
    val arr = Array(38, 27, 43, 3, 9, 82, 10)
    val sortedArr = quickSort(arr)
    println("Sorted array: " + sortedArr.mkString(", "))
  }
}
