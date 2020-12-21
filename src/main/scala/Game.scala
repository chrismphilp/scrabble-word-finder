import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/*
  Algorithm steps:

  1) Before move generation, generate all cross-checks for the row's columns
  2) Find all the anchor points for the board, and cross-checks
  3) Find all the cross-sums for each cross-check
  4) For all anchor points (feasible from rack), generate the left part of possible words
  5) For all anchor points (feasible from rack), generate the right part of possible words
  6) Combine letters multipliers as going, and compute word multipliers when end of word is found
 */

class Game(board: Board, trie: Trie, rack: Rack) {
  def initializeGame(): Unit = {

  }

  def findHighestScoringWord(): (String, Int, Int, Int, Direction.Value) = {
    var highestScoringWord: (String, Int, Int, Int, Direction.Value) = ("", 0, 0, 0, Direction.HORIZONTAL)

    for (x <- board.boardTiles.indices) {
      for (y <- board.boardTiles(x).indices) {
        val boardTile: BoardTile = board.boardTiles(x)(y)

        if (boardTile.isAnchor) {
          if (boardTile.requiresLeftCrossCheck && boardTile.horizontalCrossChecks.nonEmpty) {
            extendRight(x, y, rack.tiles, trie)
          } else {
            extendLeft(x, y, rack.tiles, 0, trie)
          }
        }
      }
    }

    @tailrec
    def extendLeft(x: Int, y: Int, letters: ListBuffer[PlayerTile], limit: Int, currTrie: Trie): Unit = {

      if (limit > 0) {
        if (board.boardTiles(x)(y).tile.nonEmpty) {
          extendLeft(x, y - 1, letters, limit - 1, currTrie)
        } else {

        }
      }
    }

    @tailrec
    def extendRight(x: Int, y: Int, letters: ListBuffer[PlayerTile], currTrie: Trie): Unit = {
      val boardTile: BoardTile = board.boardTiles(x)(y)

      if (boardTile.tile.isEmpty) {
        if (trie.isComplete)
        extendRight(x, y + 1, letters, currTrie)
      } else {

      }
    }

    highestScoringWord
  }

  def findWordPointValue(): Unit = {

  }
}
