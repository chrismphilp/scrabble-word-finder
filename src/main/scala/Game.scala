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
    board.boardTiles = GameUtilities.initialiseBoard()
  }

  def updateBoard(): Unit = board.updateBoard()

  def findHighestScoringWord(): (String, Int, Int, Int, Direction.Value) = {
    var highestScoringWord: (String, Int, Int, Int, Direction.Value) = ("", 0, 0, 0, Direction.HORIZONTAL)

    for (x <- board.boardTiles.indices) {
      for (y <- board.boardTiles(x).indices) {
        val boardTile: BoardTile = board.boardTiles(x)(y)
        var wordPoints: Int = 0

        if (boardTile.isAnchor) {
          // Horizontal words
          if (boardTile.requiresLeftCrossCheck && boardTile.horizontalCrossChecks.nonEmpty) {
            var startingPoint = y
            var startingTrie = trie

            while (startingPoint > 0 && !board.boardTiles(startingPoint)(y).isAnchor) {
              startingPoint -= 1
            }
            while (startingPoint != x) {
              startingTrie = startingTrie.children(board.boardTiles(startingPoint)(y).tile.get.letter - 65)
              wordPoints += TileUtilities.getTileScore(board.boardTiles(startingPoint)(y).tile.get.letter)
              startingPoint += 1
            }
            extendRight(x, y, x, y, rack.tiles, startingTrie, wordPoints)
          } else {
            var startingLimit = y
            while (startingLimit > 0 && !board.boardTiles(startingLimit - 1)(y).isAnchor) {
              startingLimit -= 1
            }
            extendLeft(x, y, x, y, rack.tiles, startingLimit, trie, 0)
          }

          // Vertical words
          if (boardTile.requiresAboveCrossCheck && boardTile.verticalCrossChecks.nonEmpty) {

          } else {

          }
        }
      }
    }

    @tailrec
    def extendLeft(initX: Int, initY: Int, currX: Int, currY: Int, letters: ListBuffer[PlayerTile],
                   limit: Int, currTrie: Trie, currPoints: Int): Unit = {

      extendRight(initX, initY, currX, currY, letters, currTrie, currPoints)
      if (limit > 0) {
        if (board.boardTiles(currX)(currY).tile.nonEmpty) {
          extendLeft(initX, initY, currX, currY - 1, letters, limit - 1, currTrie, currPoints)
        } else {

        }
      }
    }

    def extendRight(initX: Int, initY: Int, currX: Int, currY: Int, letters: ListBuffer[PlayerTile],
                    currTrie: Trie, currPoints: Int): Unit = {

      if (currY < board.boardTiles.length) {
        val boardTile: BoardTile = board.boardTiles(currX)(currY)

        if (boardTile.tile.isEmpty) {

          if (currTrie.isComplete && currPoints > highestScoringWord._3) {
            highestScoringWord = (currTrie.completedWord, initX, initY, currPoints, Direction.HORIZONTAL)
          }

          for (tileIndex <- letters.indices) {
            if (boardTile.verticalCrossChecks.contains(letters(tileIndex).letter) &&
              Option(currTrie.children(letters(tileIndex).letter - 65)).nonEmpty) {
              val newTrie: Trie = currTrie.children(letters(tileIndex).letter - 65)
              val newLetters = letters.clone()
              val tileScore: Int = TileUtilities.getTileScore(letters(tileIndex).letter) +
                boardTile.verticalCrossChecks(letters(tileIndex).letter)
              newLetters.remove(tileIndex)
              extendRight(initX, initY, currX, currY + 1, newLetters, newTrie, currPoints + tileScore)
            }
          }
        } else {
          if (Option(currTrie.children(boardTile.tile.get.letter - 65)).nonEmpty) {
            val newTrie: Trie = currTrie.children(boardTile.tile.get.letter - 65)
            val tileScore: Int = TileUtilities.getTileScore(boardTile.tile.get.letter)
            extendRight(initX, initY, currX, currY + 1, letters, newTrie, currPoints + tileScore)
          }
        }
      }
    }

    highestScoringWord
  }
}
