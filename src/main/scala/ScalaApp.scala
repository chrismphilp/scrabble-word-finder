object ScalaApp {
  def main(args: Array[String]): Unit = {
    val scrabbleTrie: Trie[Char] = FileReader.readFile("scrabble_words_2019.txt")
  }
}
