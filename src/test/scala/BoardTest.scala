import TileUtilities.{A, D, E, EmptyBoardTile, G, P, R}
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable

class BoardTest extends AnyFunSuite {
  val trie: Trie = FileProcessor.convertFileToTrie("collins-scrabble-words-2019.txt")

  test("Should correctly update Anchor tiles with single word") {
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
    assert(board.boardTiles(1)(2).isAnchor === true)
    assert(board.boardTiles(2)(0).isAnchor === true)
    assert(board.boardTiles(2)(4).isAnchor === true)
    assert(board.boardTiles(3)(3).isAnchor === true)

    // Tile positions
    assert(board.boardTiles(2)(1).isAnchor === false)
    assert(board.boardTiles(2)(2).isAnchor === false)
    assert(board.boardTiles(2)(3).isAnchor === false)
  }

  test("Should correctly update Anchor tiles with multiple words") {
    val board: Board = new Board(Array(
      Array(
        new BoardTile(Option(G()), Multiplier.NONE, new mutable.HashSet[Char], false,
          false, new mutable.HashSet[Char], false,
          false,false),
        new BoardTile(Option(R()), Multiplier.NONE, new mutable.HashSet[Char], false,
          false, new mutable.HashSet[Char], false,
          false,false),
        new BoardTile(Option(A()), Multiplier.NONE, new mutable.HashSet[Char], false,
          false, new mutable.HashSet[Char], false,
          false, false),
        new BoardTile(Option(D()), Multiplier.NONE, new mutable.HashSet[Char], false,
          false, new mutable.HashSet[Char], false,
          false,false),
        new BoardTile(Option(E()), Multiplier.NONE, new mutable.HashSet[Char], false,
          false, new mutable.HashSet[Char], false,
          false,false)
      ),
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
    assert(board.boardTiles(0)(1).isAnchor === false)
    assert(board.boardTiles(0)(2).isAnchor === false)
    assert(board.boardTiles(0)(3).isAnchor === false)
    assert(board.boardTiles(0)(4).isAnchor === false)

    assert(board.boardTiles(1)(0).isAnchor === true)
    assert(board.boardTiles(1)(1).isAnchor === true)
    assert(board.boardTiles(1)(2).isAnchor === true)
    assert(board.boardTiles(1)(3).isAnchor === true)
    assert(board.boardTiles(1)(4).isAnchor === true)

    assert(board.boardTiles(2)(0).isAnchor === true)
    assert(board.boardTiles(2)(1).isAnchor === false)
    assert(board.boardTiles(2)(2).isAnchor === false)
    assert(board.boardTiles(2)(3).isAnchor === false)
    assert(board.boardTiles(2)(4).isAnchor === true)

    assert(board.boardTiles(3)(0).isAnchor === false)
    assert(board.boardTiles(3)(1).isAnchor === true)
    assert(board.boardTiles(3)(2).isAnchor === true)
    assert(board.boardTiles(3)(3).isAnchor === true)
    assert(board.boardTiles(3)(4).isAnchor === false)

    assert(board.boardTiles(4)(0).isAnchor === false)
    assert(board.boardTiles(4)(1).isAnchor === false)
    assert(board.boardTiles(4)(2).isAnchor === false)
    assert(board.boardTiles(4)(3).isAnchor === false)
    assert(board.boardTiles(4)(4).isAnchor === false)
  }

  test("Should correctly update Cross-Check tiles with single word") {
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

    // Should have correct verticalCrossChecks ABOVE word
    assert(board.boardTiles(1)(0).verticalCrossChecks.size === 0)
    assert(board.boardTiles(1)(1).verticalCrossChecks.size === 15)
    assert(board.boardTiles(1)(2).verticalCrossChecks.size === 2)
    assert(board.boardTiles(1)(3).verticalCrossChecks.size === 15)
    assert(board.boardTiles(1)(4).verticalCrossChecks.size === 0)

    // Should have correct verticalCrossChecks BELOW word
    assert(board.boardTiles(3)(0).verticalCrossChecks.size === 0)
    assert(board.boardTiles(3)(1).verticalCrossChecks.size === 16)
    assert(board.boardTiles(3)(2).verticalCrossChecks.size === 4)
    assert(board.boardTiles(3)(3).verticalCrossChecks.size === 13)
    assert(board.boardTiles(3)(4).verticalCrossChecks.size === 0)

    // Should have correct horizontalCrossChecks
    assert(board.boardTiles(2)(0).horizontalCrossChecks.size === 8)
    assert(board.boardTiles(2)(1).horizontalCrossChecks.size === 0)
    assert(board.boardTiles(2)(2).horizontalCrossChecks.size === 0)
    assert(board.boardTiles(2)(3).horizontalCrossChecks.size === 0)
    assert(board.boardTiles(2)(4).horizontalCrossChecks.size === 4)
  }
}
