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
        curr.children(c - 65) = new Trie(c)
      }
      insert(curr.children(c - 65), word, step + 1)
    }
  }

  def findInitialWords(trie: Trie, tiles: ListBuffer[PlayerTile]): mutable.HashSet[String] = {
    val set: mutable.HashSet[String] = new mutable.HashSet[String]

    findWords(trie, List.range(0, tiles.length).to(ListBuffer))

    def findWords(curr: Trie, list: ListBuffer[Int]): Unit = {
      if (curr.isComplete) set += curr.completedWord
      for (x <- list.indices) {
        val tmpBuffer = list.clone()
        val tile: PlayerTile = tiles(list(x))
        tmpBuffer.remove(x)

        if (tile.score == 0) {
          for (y <- List.range(0, 26)) {
            if (Option(curr.children(y)).nonEmpty) findWords(curr.children(y), tmpBuffer)
          }
        } else if (Option(curr.children(tile.letter - 65)).nonEmpty) {
          findWords(curr.children(tile.letter - 65), tmpBuffer)
        }
      }
    }

    set
  }

  def findLongestInitialWord(trie: Trie, tiles: ListBuffer[PlayerTile]): String = {
    var longestWord: String = new String

    findLongestWord(trie, List.range(0, tiles.length).to(ListBuffer), 0)

    def findLongestWord(curr: Trie, list: ListBuffer[Int], step: Int): Unit = {
      if (curr.isComplete && (curr.completedWord.length > longestWord.length)) {
        longestWord = curr.completedWord
      }

      if (longestWord.length < (step + list.length)) {
        for (x <- list.indices) {
          val tmpBuffer = list.clone()
          val tile: PlayerTile = tiles(list(x))
          tmpBuffer.remove(x)

          if (tile.score == 0) {
            for (y <- List.range(0, 26)) {
              if (Option(curr.children(y)).nonEmpty) {
                findLongestWord(curr.children(y), tmpBuffer, step + 1)
              }
            }
          } else if (Option(curr.children(tile.letter - 65)).nonEmpty) {
            findLongestWord(curr.children(tile.letter - 65), tmpBuffer, step + 1)
          }
        }
      }
    }

    longestWord
  }
}
