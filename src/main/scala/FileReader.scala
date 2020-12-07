import scala.io.{BufferedSource, Source}

object FileReader {
  def readFile(filename: String): Trie[Char] = {
    var scrabbleTrie: ScrabbleTrie = new ScrabbleTrie
    val bufferedSource: BufferedSource = Source.fromFile(filename)
    for (line <- bufferedSource.getLines) scrabbleTrie.insertWord(line)
    bufferedSource.close()
    scrabbleTrie
  }
}
