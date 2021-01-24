import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object GreedyAlgorithm {
  def findHighestScoringWord(board: Board, trie: Trie, rack: Rack, isStartingWord: Boolean): ScoringWord = {
    val highestScoringWords: mutable.HashSet[ScoringWord] = BoardUtilities
      .findPossibleScoringWords(board, trie, rack, isStartingWord)
    if (highestScoringWords.isEmpty) {
      new ScoringWord("", 0, 0, 0, 0,
        ListBuffer.empty, Direction.HORIZONTAL, 0)
    }
    else highestScoringWords.maxBy(_.score)
  }
}
