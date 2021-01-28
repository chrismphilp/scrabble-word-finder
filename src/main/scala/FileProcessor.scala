import scala.io.Source

object FileProcessor {
  def convertFileToTrie(filename: String): Trie = {
    val trie: Trie = new Trie(' ')
    val fileSource = Source.fromFile(filename)
    for (line <- fileSource.getLines) TrieUtilities.insert(trie, line, 0)
    fileSource.close()
    trie
  }
}
