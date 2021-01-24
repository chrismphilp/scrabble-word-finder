import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object OptimalAlgorithm {
  def findOptimalPlay(rack: Rack): ScoringWord = {
    new ScoringWord("", 0, 0, 0, 0,
      ListBuffer.empty, Direction.HORIZONTAL, 0)
  }

  def updateRackHeuristics(rack: Rack, highestScoringWords: mutable.HashSet[ScoringWord]): Unit = {
    highestScoringWords.foreach(hsw => hsw.remainingRackHeuristicScore = calculateWordHeuristicScore(rack, hsw))
  }

  def calculateWordHeuristicScore(initialRack: Rack, word: ScoringWord): Double = {
    val remainingRack: ListBuffer[Char] = initialRack.tiles.clone()
      .filter(v => word.tilesUsed.contains(v))
      .map(v => v.letter)
    RackHeuristicUtilities.getRackHeuristicValue(remainingRack)
  }
}
