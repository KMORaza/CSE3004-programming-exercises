import scala.collection.mutable.PriorityQueue

// Define Huffman Tree data structures
sealed trait HuffmanTree
case class HuffmanLeaf(character: Char, frequency: Int) extends HuffmanTree
case class HuffmanNode(left: HuffmanTree, right: HuffmanTree, frequency: Int) extends HuffmanTree

object Huffman {

  // Build Huffman Tree
  def buildTree(frequencies: Map[Char, Int]): HuffmanTree = {
    val pq = PriorityQueue.empty[HuffmanTree](Ordering.by(_.asInstanceOf[HuffmanNode].frequency))

    frequencies.foreach { case (char, freq) =>
      pq.enqueue(HuffmanLeaf(char, freq))
    }

    while (pq.size > 1) {
      val left = pq.dequeue()
      val right = pq.dequeue()

      val combinedFreq = left.asInstanceOf[HuffmanLeaf].frequency + right.asInstanceOf[HuffmanLeaf].frequency
      pq.enqueue(HuffmanNode(left, right, combinedFreq))
    }

    pq.dequeue()
  }

  // Build Frequency Map
  def buildFrequencyMap(text: String): Map[Char, Int] = {
    text.groupBy(identity).mapValues(_.length)
  }

  // Build Code Table
  def buildCodeTable(tree: HuffmanTree): Map[Char, String] = {
    def buildCodeTableHelper(tree: HuffmanTree, prefix: String, codeTable: Map[Char, String]): Map[Char, String] = {
      tree match {
        case HuffmanLeaf(char, _) => codeTable + (char -> prefix)
        case HuffmanNode(left, right, _) =>
          buildCodeTableHelper(left, prefix + "0", codeTable) ++
            buildCodeTableHelper(right, prefix + "1", codeTable)
      }
    }

    buildCodeTableHelper(tree, "", Map.empty[Char, String])
  }

  // Encode text
  def encode(text: String, codeTable: Map[Char, String]): String = {
    text.flatMap(codeTable)
  }

  // Decode text
  def decode(encodedText: String, tree: HuffmanTree): String = {
    def decodeHelper(encodedText: List[Char], tree: HuffmanTree, acc: StringBuilder): String = {
      tree match {
        case HuffmanLeaf(char, _) =>
          decodeHelper(encodedText, tree, acc.append(char))
        case HuffmanNode(left, right, _) =>
          if (encodedText.head == '0')
            decodeHelper(encodedText.tail, left, acc)
          else
            decodeHelper(encodedText.tail, right, acc)
      }
    }

    decodeHelper(encodedText.toList, tree, new StringBuilder).toString
  }

  // Compress text
  def compress(text: String): (String, HuffmanTree) = {
    val frequencyMap = buildFrequencyMap(text)
    val huffmanTree = buildTree(frequencyMap)
    val codeTable = buildCodeTable(huffmanTree)
    val encodedText = encode(text, codeTable)
    (encodedText, huffmanTree)
  }

  // Decompress text
  def decompress(encodedText: String, tree: HuffmanTree): String = {
    decode(encodedText, tree)
  }

}

object HuffmanExample {
  def main(args: Array[String]): Unit = {
    val text = "this is an example for huffman encoding"

    // Compress
    val (encodedText, huffmanTree) = Huffman.compress(text)

    // Print compressed text
    println("Encoded Text: " + encodedText)

    // Decompress
    val decodedText = Huffman.decompress(encodedText, huffmanTree)

    // Print decompressed text
    println("Decoded Text: " + decodedText)
  }
}
