import scala.collection.mutable.ListBuffer

class ScoringWord(var word: String,
                  var x: Int,
                  var y: Int,
                  var score: Int,
                  var crossCheckScore: Int,
                  var tilesUsed: ListBuffer[PlayerTile],
                  var direction: Direction.Value,
                  var remainingRackHeuristicScore: Double)
