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
    val sortedWords: List[String] = buffer.toList.sorted
    sortedWords
  }
}
