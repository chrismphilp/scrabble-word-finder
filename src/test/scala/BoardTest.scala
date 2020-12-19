import TileUtilities.{A, E, EmptyBoardTile, P}
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable

class BoardTest extends AnyFunSuite {
  val trie: Trie = FileProcessor.convertFileToTrie("collins-scrabble-words-2019.txt")

  test("Should correctly update Anchor tiles") {
    val board: Board = new Board(Array(
      Array(EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile()),
      Array(EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile()),
      Array(
        EmptyBoardTile(),
        new BoardTile(Option(A()), Multiplier.NONE, new mutable.HashSet[Char], false,
          false, new mutable.HashSet[Char], false,
          false,false),
        new BoardTile(Option(P()), Multiplier.NONE, new mutable.HashSet[Char], false,
          false, new mutable.HashSet[Char], false,
          false, false),
        new BoardTile(Option(E()), Multiplier.NONE, new mutable.HashSet[Char], false,
          false, new mutable.HashSet[Char], false,
          false,false),
        EmptyBoardTile()
      ),
      Array(EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile()),
      Array(EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile())
    ), trie)

    board.updateBoard()

    assert(board.boardTiles(0)(0).isAnchor === false)
    assert(board.boardTiles(1)(0).isAnchor === false)
    assert(board.boardTiles(1)(1).isAnchor === true)
    assert(board.boardTiles(1)(1).isAnchor === true)
    assert(board.boardTiles(2)(0).isAnchor === true)
    assert(board.boardTiles(2)(4).isAnchor === true)
    assert(board.boardTiles(3)(3).isAnchor === true)

    // Tile positions
    assert(board.boardTiles(2)(1).isAnchor === false)
    assert(board.boardTiles(2)(2).isAnchor === false)
    assert(board.boardTiles(2)(3).isAnchor === false)
  }
}
