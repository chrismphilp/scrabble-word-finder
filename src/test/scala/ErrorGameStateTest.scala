import GameUtilities.initialiseBoard
import TileUtilities._
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable.ListBuffer

class ErrorGameStateTest extends AnyFunSuite {
  val trie: Trie = FileProcessor.convertFileToTrie("collins-scrabble-words-2019.txt")
  val rack: Rack = new Rack(new ListBuffer[PlayerTile])
  val bag: Bag = createInitialBag()

  test("Should pass Error game state 1") {
    val board: Board = new Board(initialiseBoard(), trie)

    board.boardTiles(7)(7) = FilledBoardTile(V())
    board.boardTiles(7)(8) = FilledBoardTile(I())
    board.boardTiles(7)(9) = FilledBoardTile(B())
    board.boardTiles(7)(10) = FilledBoardTile(E())
    board.boardTiles(7)(11) = FilledBoardTile(Y())

    board.boardTiles(7)(9) = FilledBoardTile(B())
    board.boardTiles(8)(9) = FilledBoardTile(A())
    board.boardTiles(9)(9) = FilledBoardTile(C())
    board.boardTiles(10)(9) = FilledBoardTile(L())
    board.boardTiles(11)(9) = FilledBoardTile(A())
    board.boardTiles(12)(9) = FilledBoardTile(V())
    board.boardTiles(13)(9) = FilledBoardTile(A())

    board.boardTiles(11)(7) = FilledBoardTile(G())
    board.boardTiles(11)(8) = FilledBoardTile(L())
    board.boardTiles(11)(9) = FilledBoardTile(A())
    board.boardTiles(11)(10) = FilledBoardTile(I())
    board.boardTiles(11)(11) = FilledBoardTile(K())
    board.boardTiles(11)(12) = FilledBoardTile(E())
    board.boardTiles(11)(13) = FilledBoardTile(T())

    rack.setRack(List(G(), C(), D(), N(), O(), E(), E()).to(ListBuffer))

    val game: Game = new Game(board, trie, rack, bag)
    game.printBoard()
    game.updateBoard()

    assert(board.boardTiles.length === 15)
    assert(board.boardTiles(0).length === 15)

    assert(board.boardTiles(6)(9).requiresBelowCrossCheck === true)
    assert(board.boardTiles(6)(9).requiresAboveCrossCheck === false)
    assert(board.boardTiles(6)(9).requiresLeftCrossCheck === false)
    assert(board.boardTiles(6)(9).requiresRightCrossCheck === false)
    assert(board.boardTiles(6)(9).verticalCrossChecks.contains('O') === false)

    val highestScoringWord: HighestScoringWord = game.findHighestScoringWord(false)

    assert(board.boardTiles(10)(7).requiresBelowCrossCheck === true)
    assert(highestScoringWord.word === "COGGED")
  }

  test("Should pass Error game state 2") {
    val board: Board = new Board(initialiseBoard(), trie)

    board.boardTiles(7)(7) = FilledBoardTile(V())
    board.boardTiles(7)(8) = FilledBoardTile(I())
    board.boardTiles(7)(9) = FilledBoardTile(B())
    board.boardTiles(7)(10) = FilledBoardTile(E())
    board.boardTiles(7)(11) = FilledBoardTile(Y())

    board.boardTiles(7)(9) = FilledBoardTile(B())
    board.boardTiles(8)(9) = FilledBoardTile(A())
    board.boardTiles(9)(9) = FilledBoardTile(C())

    board.boardTiles(9)(6) = FilledBoardTile(D())
    board.boardTiles(9)(7) = FilledBoardTile(A())
    board.boardTiles(9)(8) = FilledBoardTile(N())
    board.boardTiles(9)(9) = FilledBoardTile(C())
    board.boardTiles(9)(10) = FilledBoardTile(E())

    rack.setRack(List(E(), N(), C(), L(), A(), V(), E()).to(ListBuffer))

    val game: Game = new Game(board, trie, rack, bag)
    game.printBoard()
    game.updateBoard()

    assert(board.boardTiles.length === 15)
    assert(board.boardTiles(0).length === 15)

    val highestScoringWord: HighestScoringWord = game.findHighestScoringWord(false)

    assert(List("VALENCE", "ENCLAVE").contains(highestScoringWord.word))
    assert(highestScoringWord.x === 10)
    assert(highestScoringWord.y === 0)
  }
}
