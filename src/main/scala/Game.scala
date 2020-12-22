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

  def findHighestScoringWord(): HighestScoringWord = {
    val highestScoringWord: HighestScoringWord =
      new HighestScoringWord("", 0, 0, 0, Direction.HORIZONTAL)

    for (x <- board.boardTiles.indices) {
      for (y <- board.boardTiles(x).indices) {
        val boardTile: BoardTile = board.boardTiles(x)(y)
        var wordPoints: Int = 0

        if (boardTile.isAnchor) {
          // Horizontal words
          if (boardTile.requiresLeftCrossCheck && boardTile.horizontalCrossChecks.nonEmpty) {
            var startingPoint = y
            var startingTrie = trie

            while (startingPoint > 0 && !board.boardTiles(x)(startingPoint - 1).isAnchor) {
              startingPoint -= 1
            }
            while (startingPoint != y) {
              startingTrie = startingTrie.children(board.boardTiles(x)(startingPoint).tile.get.letter - 65)
              wordPoints += TileUtilities.getTileScore(board.boardTiles(x)(startingPoint).tile.get.letter)
              startingPoint += 1
            }
            extendRight(x, y, x, y, rack.tiles, startingTrie, wordPoints)
          } else {
            var startingPoint = y
            var startingLimit = 0
            while (startingLimit > 0 && !board.boardTiles(x)(startingPoint - 1).isAnchor) {
              startingPoint -= 1
              startingLimit += 1
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
        extendLeft(initX, initY, currX, currY - 1, letters, limit - 1, currTrie, currPoints)
      }
    }

    def extendRight(initX: Int, initY: Int, currX: Int, currY: Int, letters: ListBuffer[PlayerTile],
                    currTrie: Trie, currPoints: Int): Unit = {

      if (currY < board.boardTiles.length) {
        val boardTile: BoardTile = board.boardTiles(currX)(currY)

        if (boardTile.tile.isEmpty) {

          if (currTrie.isComplete && currPoints > highestScoringWord.score) {
            highestScoringWord.word = currTrie.completedWord
            highestScoringWord.score = currPoints
            highestScoringWord.x = initX
            highestScoringWord.y = initY
            highestScoringWord.direction = Direction.HORIZONTAL
          }

          for (tileIndex <- letters.indices) {
            val rackTile: PlayerTile = letters(tileIndex)

            if (!boardTile.requiresAboveCrossCheck && !boardTile.requiresBelowCrossCheck) {
              val newTrie: Trie = currTrie.children(rackTile.letter - 65)
              val newLetters = letters.clone()
              val tileScore: Int = TileUtilities.getTileScore(rackTile.letter)
              newLetters.remove(tileIndex)
              extendRight(initX, initY, currX, currY + 1, newLetters, newTrie, currPoints + tileScore)
            } else if (boardTile.verticalCrossChecks.contains(rackTile.letter) &&
              Option(currTrie.children(rackTile.letter - 65)).nonEmpty) {
              val newTrie: Trie = currTrie.children(rackTile.letter - 65)
              val newLetters = letters.clone()
              val tileScore: Int = TileUtilities.getTileScore(rackTile.letter) +
                boardTile.verticalCrossChecks(rackTile.letter)
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
      } else if (Option(currTrie).nonEmpty && currTrie.isComplete &&
        currPoints > highestScoringWord.score) {
        highestScoringWord.word = currTrie.completedWord
        highestScoringWord.score = currPoints
        highestScoringWord.x = initX
        highestScoringWord.y = initY
        highestScoringWord.direction = Direction.HORIZONTAL
      }
    }

    highestScoringWord
  }
}
