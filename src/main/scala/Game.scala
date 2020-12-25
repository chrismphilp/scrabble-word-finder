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
            extendRight(x, y, x, y, rack.tiles, startingTrie, wordPoints, new ListBuffer[Multiplier.Value])
          } else {
            var startingPoint = y
            var startingLimit = 0
            while (startingPoint > 0 && !board.boardTiles(x)(startingPoint - 1).isAnchor) {
              startingPoint -= 1
              startingLimit += 1
            }
            extendLeft(x, y, x, y, rack.tiles, startingLimit, trie, 0)
          }

          // Vertical words
          if (boardTile.requiresAboveCrossCheck && boardTile.verticalCrossChecks.nonEmpty) {
            var startingPoint = x
            var startingTrie = trie

            while (startingPoint > 0 && !board.boardTiles(startingPoint - 1)(y).isAnchor) {
              startingPoint -= 1
            }
            while (startingPoint != x) {
              startingTrie = startingTrie.children(board.boardTiles(startingPoint)(y).tile.get.letter - 65)
              wordPoints += TileUtilities.getTileScore(board.boardTiles(startingPoint)(y).tile.get.letter)
              startingPoint += 1
            }
            extendBelow(x, y, x, y, rack.tiles, startingTrie, wordPoints, new ListBuffer[Multiplier.Value])
          } else {
            var startingPoint = x
            var startingLimit = 0

            while (startingPoint > 0 && !board.boardTiles(startingPoint - 1)(y).isAnchor) {
              startingPoint -= 1
              startingLimit += 1
            }
            extendAbove(x, y, x, y, rack.tiles, startingLimit, trie, 0)
          }
        }
      }
    }

    @tailrec
    def extendLeft(initX: Int, initY: Int, currX: Int, currY: Int, letters: ListBuffer[PlayerTile],
                   limit: Int, currTrie: Trie, currPoints: Int): Unit = {

      extendRight(initX, initY, currX, currY, letters, currTrie, currPoints, new ListBuffer[Multiplier.Value])
      if (limit > 0) {
        extendLeft(initX, initY - 1, currX, currY - 1, letters, limit - 1, currTrie, currPoints)
      }
    }

    def extendRight(initX: Int, initY: Int, currX: Int, currY: Int, letters: ListBuffer[PlayerTile],
                    currTrie: Trie, currPoints: Int, bonuses: ListBuffer[Multiplier.Value]): Unit = {

      val totalPoints = currentPoints(currPoints, bonuses)
      var tmpBonuses: ListBuffer[Multiplier.Value] = bonuses.clone()

      if (Option(currTrie).nonEmpty && currY < board.boardTiles.length) {
        val boardTile: BoardTile = board.boardTiles(currX)(currY)

        if (boardTile.tile.isEmpty) {

          if (currTrie.isComplete && totalPoints > highestScoringWord.score) {
            setHighestScoringWord(highestScoringWord, currTrie.completedWord, totalPoints,
              initX, initY, Direction.HORIZONTAL)
          }

          boardTile.multiplier match {
            case Multiplier.DOUBLE_WORD => tmpBonuses += boardTile.multiplier
            case Multiplier.TRIPLE_WORD => tmpBonuses += boardTile.multiplier
            case _ =>
          }

          for (tileIndex <- letters.indices) {
            val rackTile: PlayerTile = letters(tileIndex)
            val newTrie: Trie = currTrie.children(rackTile.letter - 65)
            val newLetters = letters.clone()
            var tileScore: Int = 0
            newLetters.remove(tileIndex)

            if (!boardTile.requiresAboveCrossCheck && !boardTile.requiresBelowCrossCheck) {
              tileScore = TileUtilities.getTileScore(rackTile.letter) *
                TileUtilities.getTileMultiplierValue(boardTile.multiplier)
            } else if (boardTile.verticalCrossChecks.contains(rackTile.letter) &&
              Option(currTrie.children(rackTile.letter - 65)).nonEmpty) {
              tileScore = (TileUtilities.getTileScore(rackTile.letter) *
                TileUtilities.getTileMultiplierValue(boardTile.multiplier)) +
                boardTile.verticalCrossChecks(rackTile.letter)
            }
            extendRight(initX, initY, currX, currY + 1, newLetters, newTrie, currPoints + tileScore, tmpBonuses)
          }
        } else if (Option(currTrie.children(boardTile.tile.get.letter - 65)).nonEmpty) {
          val newTrie: Trie = currTrie.children(boardTile.tile.get.letter - 65)
          val tileScore: Int = TileUtilities.getTileScore(boardTile.tile.get.letter)
          extendRight(initX, initY, currX, currY + 1, letters, newTrie, currPoints + tileScore, tmpBonuses)
        }
      } else if (Option(currTrie).nonEmpty && currTrie.isComplete &&
        totalPoints > highestScoringWord.score) {
        setHighestScoringWord(highestScoringWord, currTrie.completedWord, totalPoints,
          initX, initY, Direction.HORIZONTAL)
      }
    }

    @tailrec
    def extendAbove(initX: Int, initY: Int, currX: Int, currY: Int, letters: ListBuffer[PlayerTile],
                    limit: Int, currTrie: Trie, currPoints: Int): Unit = {

      extendBelow(initX, initY, currX, currY, letters, currTrie, currPoints, new ListBuffer[Multiplier.Value])
      if (limit > 0) {
        extendAbove(initX - 1, initY, currX - 1, currY, letters, limit - 1, currTrie, currPoints)
      }
    }

    def extendBelow(initX: Int, initY: Int, currX: Int, currY: Int, letters: ListBuffer[PlayerTile],
                    currTrie: Trie, currPoints: Int, bonuses: ListBuffer[Multiplier.Value]): Unit = {

      val totalPoints = currentPoints(currPoints, bonuses)
      var tmpBonuses: ListBuffer[Multiplier.Value] = bonuses.clone()

      if (Option(currTrie).nonEmpty && currX < board.boardTiles.length) {
        val boardTile: BoardTile = board.boardTiles(currX)(currY)

        if (boardTile.tile.isEmpty) {

          if (currTrie.isComplete && totalPoints > highestScoringWord.score) {
            setHighestScoringWord(highestScoringWord, currTrie.completedWord, totalPoints,
              initX, initY, Direction.VERTICAL)
          }

          boardTile.multiplier match {
            case Multiplier.DOUBLE_WORD => tmpBonuses += boardTile.multiplier
            case Multiplier.TRIPLE_WORD => tmpBonuses += boardTile.multiplier
            case _ =>
          }

          for (tileIndex <- letters.indices) {
            val rackTile: PlayerTile = letters(tileIndex)
            val newTrie: Trie = currTrie.children(rackTile.letter - 65)
            val newLetters = letters.clone()
            newLetters.remove(tileIndex)

            if (!boardTile.requiresLeftCrossCheck && !boardTile.requiresRightCrossCheck) {
              val tileScore: Int = TileUtilities.getTileScore(rackTile.letter) *
                TileUtilities.getTileMultiplierValue(boardTile.multiplier)
              extendBelow(initX, initY, currX + 1, currY, newLetters, newTrie, currPoints + tileScore, tmpBonuses)
            } else if (boardTile.horizontalCrossChecks.contains(rackTile.letter) &&
              Option(currTrie.children(rackTile.letter - 65)).nonEmpty) {
              val tileScore: Int = (TileUtilities.getTileScore(rackTile.letter) *
                TileUtilities.getTileMultiplierValue(boardTile.multiplier)) +
                boardTile.horizontalCrossChecks(rackTile.letter)
              extendBelow(initX, initY, currX + 1, currY, newLetters, newTrie, currPoints + tileScore, tmpBonuses)
            }
          }
        } else if (Option(currTrie.children(boardTile.tile.get.letter - 65)).nonEmpty) {
          val newTrie: Trie = currTrie.children(boardTile.tile.get.letter - 65)
          val tileScore: Int = TileUtilities.getTileScore(boardTile.tile.get.letter)
          extendBelow(initX, initY, currX + 1, currY, letters, newTrie, currPoints + tileScore, tmpBonuses)
        }
      } else if (Option(currTrie).nonEmpty && currTrie.isComplete &&
        totalPoints > highestScoringWord.score) {
        setHighestScoringWord(highestScoringWord, currTrie.completedWord, totalPoints,
          initX, initY, Direction.VERTICAL)
      }
    }

    highestScoringWord
  }


  def currentPoints(score: Int, bonuses: ListBuffer[Multiplier.Value]): Int = {
    score * bonuses.map(v => TileUtilities.getWordMultiplierValue(v)).product
  }

  def setHighestScoringWord(highestScoringWord: HighestScoringWord, completedWord: String,
                            score: Int, initX: Int, initY: Int, direction: Direction.Value): Unit = {
    highestScoringWord.word = completedWord
    highestScoringWord.score = score
    highestScoringWord.x = initX
    highestScoringWord.y = initY
    highestScoringWord.direction = direction
  }
}
