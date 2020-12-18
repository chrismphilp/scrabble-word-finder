import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class RackTest extends AnyFunSuite {
  val trie: Trie = FileProcessor.convertFileToTrie("collins-scrabble-words-2019.txt")
  val bag: Bag = TileUtilities.createInitialBag()
  val rack: Rack = new Rack(new ListBuffer[Tile])

  test("Should correctly fill initial Rack") {
    rack.fillRack(bag)
    assert(rack.tiles.length === 7)
    rack.tiles.head.score should be >= 0
  }

  test("Should correctly find all valid initial words") {
    rack.fillRack(bag)
    val buffer: mutable.HashSet[String] = TrieUtilities.findInitialWords(trie, rack.tiles)

    for (x <- buffer) assert(TrieUtilities.search(trie, x, 0) === true)
  }

  test("Should find longest word") {
    rack.fillRack(bag)
    val longestWord: String = TrieUtilities.findLongestInitialWord(trie, rack.tiles)

    assert(TrieUtilities.search(trie, longestWord, 0) === true)
  }

  test("Should find highest scoring word") {
    rack.fillRack(bag)
    val highestScoringWord: (String, Int) = TrieUtilities.findHighestScoringInitialWord(trie, rack.tiles)

    assert(TrieUtilities.search(trie, highestScoringWord._1, 0) === true)
    assert(highestScoringWord._2 > 0 === true)
  }

  test("Should find longest and highest scoring word") {
    rack.fillRack(bag)
    val highestScoringWord: (String, Int) = TrieUtilities.findHighestScoringInitialWord(trie, rack.tiles)

    assert(TrieUtilities.search(trie, highestScoringWord._1, 0) === true)
    assert(highestScoringWord._2 > 0 === true)
  }
}
