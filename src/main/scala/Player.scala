import scala.collection.mutable.ListBuffer

class Player(val name: String, var rack: Rack, var placedWords: ListBuffer[HighestScoringWord],
             val algorithm: Algorithm.Value = Algorithm.GREEDY, var score: Int = 0)
