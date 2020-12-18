import org.scalatest.funsuite.AnyFunSuite

class FileProcessorTest extends AnyFunSuite {
  val trie: Trie = FileProcessor.convertFileToTrie("collins-scrabble-words-2019.txt")
  test("Power digit sum test") {
    assert(TrieUtilities.search(trie, "AARDVARK", 0) === true)
    assert(TrieUtilities.search(trie, "HE", 0) === true)
    assert(TrieUtilities.search(trie, "HEL", 0) === false)
    assert(TrieUtilities.search(trie, "HELL", 0) === true)
    assert(TrieUtilities.search(trie, "HELLO", 0) === true)
    assert(TrieUtilities.search(trie, "HELLOS", 0) === true)
    assert(TrieUtilities.search(trie, "HELLOSS", 0) === false)
  }
}
