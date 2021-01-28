import GameUtilities.initialiseBoard
import TileUtilities._
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable.ListBuffer

class OptimalAlgorithmTest extends AnyFunSuite {
  val trie: Trie = FileProcessor.convertFileToTrie("collins-scrabble-words-2019.txt")

  test("Should assign correct remaining rack heuristic value for non-duplicated letters") {
    val board: Board = new Board(initialiseBoard(), trie)
    val rack: Rack = new Rack()
    rack.setRack(List(A(), P(), E(), R(), Z()).to(ListBuffer))

    var highestScoringWord: ScoringWord = OptimalAlgorithm
      .findHighestScoringWord(board, trie, rack)
    assertResult(6.5)(highestScoringWord.remainingRackHeuristicScore)

    rack.setRack(List(P(), Y(), X(), S(), M(), A()).to(ListBuffer))
    highestScoringWord = OptimalAlgorithm.findHighestScoringWord(board, trie, rack)
    assertResult(8.5)(highestScoringWord.remainingRackHeuristicScore)
  }

  test("Should assign correct remaining rack heuristic value for duplicated letters") {
    val board: Board = new Board(initialiseBoard(), trie)
    val rack: Rack = new Rack()
    rack.setRack(List(P(), Y(), R(), E(), X(), A(), A()).to(ListBuffer))

    var highestScoringWord: ScoringWord = OptimalAlgorithm
      .findHighestScoringWord(board, trie, rack)
    assertResult(-3.0)(highestScoringWord.remainingRackHeuristicScore)

    rack.setRack(List(A(), A(), A(), A(), A()).to(ListBuffer))

    highestScoringWord = OptimalAlgorithm.findHighestScoringWord(board, trie, rack)
    assertResult(-9.0)(highestScoringWord.remainingRackHeuristicScore)
  }
}
