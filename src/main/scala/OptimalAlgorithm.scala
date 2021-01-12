import scala.collection.mutable.ListBuffer

object OptimalAlgorithm {
  def findOptimalPlay(): HighestScoringWord = {
    new HighestScoringWord("", 0, 0, 0, 0, ListBuffer.empty, Direction.HORIZONTAL)
  }
}
