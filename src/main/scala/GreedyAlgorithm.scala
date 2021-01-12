import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object GreedyAlgorithm {
  def findHighestScoringWord(board: Board, trie: Trie, rack: Rack, isStartingWord: Boolean): HighestScoringWord = {
    val highestScoringWords: mutable.HashSet[HighestScoringWord] = BoardUtilities
      .findPossibleScoringWords(board, trie, rack, isStartingWord)
    if (highestScoringWords.isEmpty) {
      new HighestScoringWord("", 0, 0, 0, 0, ListBuffer.empty, Direction.HORIZONTAL)
    }
    else highestScoringWords.maxBy(_.score)
  }
}
