import TileUtilities._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class RackTest extends AnyFunSuite {
  val trie: Trie = FileProcessor.convertFileToTrie("collins-scrabble-words-2019.txt")
  val bag: Bag = TileUtilities.createInitialBag()
  val rack: Rack = new Rack()

  test("Should correctly fill initial Rack") {
    rack.fillRack(bag)
    assert(rack.tiles.length === 7)
    rack.tiles.head.score should be >= 0
  }

  test("Should correctly find all valid initial words") {
    rack.fillRack(bag)
    val words: mutable.HashSet[String] = TrieUtilities.findInitialWords(trie, rack.tiles)

    for (x <- words) assert(TrieUtilities.search(trie, x, 0) === true)
  }

  test("Should correctly find all valid initial words from pre-made set") {
    rack.setRack(List(Z(), Z(), A(), A(), R(), R(), R()).to(ListBuffer))
    val words: mutable.HashSet[String] = TrieUtilities.findInitialWords(trie, rack.tiles)

    assert(words.size === 5)
    for (x <- words) assert(TrieUtilities.search(trie, x, 0) === true)
  }

  test("Should find longest word") {
    rack.fillRack(bag)
    val longestWord: String = TrieUtilities.findLongestInitialWord(trie, rack.tiles)

    assert(TrieUtilities.search(trie, longestWord, 0) === true)
  }

  test("Should find longest word (WART) from pre-made rack") {
    rack.setRack(List(W(), A(), R(), T()).to(ListBuffer))
    var longestWord: String = TrieUtilities.findLongestInitialWord(trie, rack.tiles)

    assert(longestWord === "WART")
    assert(TrieUtilities.search(trie, longestWord, 0) === true)

    rack.setRack(List(R(), A(), T(), W()).to(ListBuffer))
    longestWord = TrieUtilities.findLongestInitialWord(trie, rack.tiles)

    assert(longestWord === "WART")
    assert(TrieUtilities.search(trie, longestWord, 0) === true)
  }

  test("Should find longest word (SWAGGIE) from pre-made rack") {
    rack.setRack(List(S(), E(), A(), G(), W(), I(), G()).to(ListBuffer))
    val longestWord: String = TrieUtilities.findLongestInitialWord(trie, rack.tiles)

    assert(longestWord === "SWAGGIE")
    assert(TrieUtilities.search(trie, longestWord, 0) === true)
  }
}
