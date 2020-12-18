import TileUtilities.{A, E, EmptyBoardTile, P}
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable.ListBuffer

class BoardTest extends AnyFunSuite {
  test("Should correctly update Anchor tiles") {
    val board: Board = new Board(Array(
      Array(EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile()),
      Array(EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile()),
      Array(
        EmptyBoardTile(),
        new BoardTile(Option(A()), Multiplier.NONE, new ListBuffer[PlayerTile], false),
        new BoardTile(Option(P()), Multiplier.NONE, new ListBuffer[PlayerTile], false),
        new BoardTile(Option(E()), Multiplier.NONE, new ListBuffer[PlayerTile], false),
        EmptyBoardTile()
      ),
      Array(EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile()),
      Array(EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile())
    ))
    board.updateAnchorTiles()

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
