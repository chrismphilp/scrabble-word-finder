import scala.collection.mutable.ListBuffer

class Player(val name: String,
             val algorithm: Algorithm.Value,
             var rack: Rack = new Rack(),
             var placedWords: ListBuffer[ScoringWord] = new ListBuffer[ScoringWord],
             var score: Int = 0)
