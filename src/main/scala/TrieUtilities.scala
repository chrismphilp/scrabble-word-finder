import scala.annotation.tailrec

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
    } else {
      val c = word.charAt(step)
      if (curr.children(c - 65) == null) {
        curr.children(c - 65) = new Trie(c, false, new Array[Trie](26))
      }
      insert(curr.children(c - 65), word, step + 1)
    }
  }
}
