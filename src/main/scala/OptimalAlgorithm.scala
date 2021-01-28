import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object OptimalAlgorithm {
  def findHighestScoringWord(board: Board, trie: Trie, rack: Rack): ScoringWord = {
    val scoringWords: mutable.HashSet[ScoringWord] = BoardUtilities
      .findPossibleScoringWords(board, trie, rack)
    updateRackHeuristics(rack, scoringWords)
    if (scoringWords.isEmpty) {
      new ScoringWord("", 0, 0, 0, 0,
        ListBuffer.empty, Direction.HORIZONTAL, 0)
    }
    else scoringWords.maxBy(v => v.score + v.remainingRackHeuristicScore)
  }

  def updateRackHeuristics(rack: Rack, scoringWords: mutable.HashSet[ScoringWord]): Unit = {
    scoringWords.foreach(hsw => hsw.remainingRackHeuristicScore = calculateWordHeuristicScore(rack, hsw))
  }

  def calculateWordHeuristicScore(initialRack: Rack, word: ScoringWord): Double = {
    val remainingRack: ListBuffer[Char] = initialRack.tiles.clone()
      .filter(!word.tilesUsed.contains(_))
      .map(_.letter)
    RackHeuristicUtilities.getTotalHeuristicValue(remainingRack)
  }
}
