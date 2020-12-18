import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object TrieUtilities {

  @tailrec
  def search(curr: Trie, word: String, step: Int): Boolean = {
    if (word.length == step) curr.isComplete
    else {
      val c = word.charAt(step)
      if (curr.children(c - 65) == null) false
      else search(curr.children(c - 65), word, step + 1)
    }
  }

  @tailrec
  def insert(curr: Trie, word: String, step: Int): Unit = {
    if (word.length == step) {
      curr.isComplete = true
      curr.completedWord = word
    } else {
      val c = word.charAt(step)
      if (Option(curr.children(c - 65)).isEmpty) {
        curr.children(c - 65) = new Trie(c, false, "", new Array[Trie](26))
      }
      insert(curr.children(c - 65), word, step + 1)
    }
  }

  def findInitialWords(trie: Trie, tiles: ListBuffer[Tile]): mutable.HashSet[String] = {
    val set: mutable.HashSet[String] = new mutable.HashSet[String]

    findWord(trie, List.range(0, 7).to(ListBuffer))

    def findWord(curr: Trie, list: ListBuffer[Int]): Unit = {
      if (curr.isComplete) set += curr.completedWord
      for (x <- list.indices) {
        val tmpBuffer = list.clone()
        val tile: Tile = tiles(list(x))

        if (tile.score == 0) {
          tmpBuffer.remove(x)
          for (y <- List.range(0, 26)) {
            if (Option(curr.children(y)).nonEmpty) findWord(curr.children(y), tmpBuffer)
          }
        } else {
          if (Option(curr.children(tile.letter - 65)).nonEmpty) {
            tmpBuffer.remove(x)
            findWord(curr.children(tiles(list(x)).letter - 65), tmpBuffer)
          }
        }
      }
    }
    set
  }
}
