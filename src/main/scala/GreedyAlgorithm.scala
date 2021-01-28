import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object GreedyAlgorithm {
  def findHighestScoringWord(board: Board, trie: Trie, rack: Rack): ScoringWord = {
    val scoringWords: mutable.HashSet[ScoringWord] = BoardUtilities
      .findPossibleScoringWords(board, trie, rack)
    if (scoringWords.isEmpty) {
      new ScoringWord("", 0, 0, 0, 0,
        ListBuffer.empty, Direction.HORIZONTAL, 0)
    }
    else scoringWords.maxBy(_.score)
  }
}
