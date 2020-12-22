import TileUtilities._
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class GameTest extends AnyFunSuite {
  val trie: Trie = FileProcessor.convertFileToTrie("collins-scrabble-words-2019.txt")
  val rack: Rack = new Rack(new ListBuffer[PlayerTile])

  test("Should correctly find highest scoring word from single word") {
    val board: Board = new Board(Array(
      Array(EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile()),
      Array(EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile()),
      Array(
        EmptyBoardTile(),
        new BoardTile(Option(A()), Multiplier.NONE, new mutable.HashMap[Char, Int], false,
          false, new mutable.HashMap[Char, Int], false,
          false, false),
        new BoardTile(Option(P()), Multiplier.NONE, new mutable.HashMap[Char, Int], false,
          false, new mutable.HashMap[Char, Int], false,
          false, false),
        new BoardTile(Option(E()), Multiplier.NONE, new mutable.HashMap[Char, Int], false,
          false, new mutable.HashMap[Char, Int], false,
          false, false),
        EmptyBoardTile()
      ),
      Array(EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile()),
      Array(EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile())
    ), trie)

    rack.setRack(List(A(), P(), E(), R()).to(ListBuffer))

    val game: Game = new Game(board, trie, rack)
    game.updateBoard()

    val highestScoringWord: HighestScoringWord = game.findHighestScoringWord()

    assert(highestScoringWord.word === "")
  }
}
