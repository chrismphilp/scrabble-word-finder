import TileUtilities.FilledBoardTile

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

class Game(board: Board, trie: Trie, rack: Rack, bag: Bag) {

  def initializeGame(): Unit = {
    rack.fillRack(bag)
    board.boardTiles = GameUtilities.initialiseBoard()
  }

  def updateBoard(): Unit = board.updateBoard()

  def updateRack(): Unit = rack.fillRack(bag)

  def findHighestScoringWord(isStartingWord: Boolean): HighestScoringWord = {
    val highestScoringWord: HighestScoringWord =
      new HighestScoringWord("", 0, 0, 0, 0, Direction.HORIZONTAL)

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
            val initY = startingPoint
            while (startingPoint != y) {
              startingTrie = startingTrie.children(board.boardTiles(x)(startingPoint).tile.get.letter - 65)
              wordPoints += TileUtilities.getTileScore(board.boardTiles(x)(startingPoint).tile.get.letter)
              startingPoint += 1
            }
            updatedExtendRight(x, initY, x, y, rack.tiles, startingTrie, wordPoints,
              0, rack.tiles.length, new ListBuffer[Multiplier.Value], 0)
          } else if (!boardTile.requiresLeftCrossCheck) {
            var startingPoint = y
            var startingLimit = 0
            while (startingPoint > 0 && startingLimit < rack.tiles.length - 1 &&
              !board.boardTiles(x)(startingPoint - 1).isAnchor) {
              startingPoint -= 1
              startingLimit += 1
            }
            extendLeft(x, y, x, y, rack.tiles, startingLimit, 0, trie,
              0, isStartingWord)
          }

          // Vertical words
          if (boardTile.requiresAboveCrossCheck && boardTile.verticalCrossChecks.nonEmpty) {
            var startingPoint = x
            var startingTrie = trie

            while (startingPoint > 0 && !board.boardTiles(startingPoint - 1)(y).isAnchor) {
              startingPoint -= 1
            }
            val initX = startingPoint
            while (startingPoint != x) {
              startingTrie = startingTrie.children(board.boardTiles(startingPoint)(y).tile.get.letter - 65)
              wordPoints += TileUtilities.getTileScore(board.boardTiles(startingPoint)(y).tile.get.letter)
              startingPoint += 1
            }
            updatedExtendBelow(initX, y, x, y, rack.tiles, startingTrie, wordPoints,
              0, rack.tiles.length, new ListBuffer[Multiplier.Value], 0)
          } else if (!boardTile.requiresAboveCrossCheck) {
            var startingPoint = x
            var startingLimit = 0

            while (startingPoint > 0 && startingLimit < rack.tiles.length - 1 &&
              !board.boardTiles(startingPoint - 1)(y).isAnchor) {
              startingPoint -= 1
              startingLimit += 1
            }
            extendAbove(x, y, x, y, rack.tiles, startingLimit, 0,
              trie, 0, isStartingWord)
          }
        }
      }
    }

    @tailrec
    def extendLeft(initX: Int, initY: Int, currX: Int, currY: Int, letters: ListBuffer[PlayerTile],
                   limit: Int, distanceFromOrigin: Int, currTrie: Trie, currPoints: Int,
                   isStartingWord: Boolean): Unit = {

      updatedExtendRight(initX, initY, currX, currY, letters, currTrie, currPoints,
        0, letters.length, new ListBuffer[Multiplier.Value],
        if (isStartingWord) 0 else distanceFromOrigin)
      if (limit > 0) {
        extendLeft(initX, initY - 1, currX, currY - 1, letters, limit - 1, distanceFromOrigin + 1,
          currTrie, currPoints, isStartingWord)
      }
    }

    def updatedExtendRight(initX: Int, initY: Int, currX: Int, currY: Int, letters: ListBuffer[PlayerTile],
                           currTrie: Trie, currPoints: Int, crossCheckPoints: Int, initialRackSize: Int,
                           bonuses: ListBuffer[Multiplier.Value], limitNumber: Int): Unit = {
      if (Option(currTrie).nonEmpty) {

        val totalPoints = currentPoints(currPoints, bonuses) + crossCheckPoints +
          (if (initialRackSize == 7 && letters.isEmpty) 50 else 0)
        var tmpBonuses: ListBuffer[Multiplier.Value] = bonuses.clone()

        if (currY == board.boardTiles.length) {
          if (currTrie.isComplete && totalPoints > highestScoringWord.score) {
            setHighestScoringWord(highestScoringWord, currTrie.completedWord, totalPoints, crossCheckPoints,
              initX, initY, Direction.HORIZONTAL)
          }
        } else {
          val boardTile: BoardTile = board.boardTiles(currX)(currY)

          if (boardTile.tile.isEmpty) {
            if (limitNumber < 0 && letters.length < initialRackSize &&
              currTrie.isComplete &&
              totalPoints > highestScoringWord.score) {
              setHighestScoringWord(highestScoringWord, currTrie.completedWord, totalPoints, crossCheckPoints,
                initX, initY, Direction.HORIZONTAL)
            }

            boardTile.multiplier match {
              case Multiplier.DOUBLE_WORD => tmpBonuses += boardTile.multiplier
              case Multiplier.TRIPLE_WORD => tmpBonuses += boardTile.multiplier
              case _ =>
            }

            for (tileIndex <- letters.indices) {
              val rackTile: PlayerTile = letters(tileIndex)
              var tileScore: Int = 0
              var crossCheckScore: Int = 0

              val newTrie: Trie = currTrie.children(rackTile.letter - 65)
              val newLetters = letters.clone()

              newLetters.remove(tileIndex)

              if (!boardTile.requiresAboveCrossCheck && !boardTile.requiresBelowCrossCheck) {
                tileScore = TileUtilities.getTileScore(rackTile.letter) *
                  TileUtilities.getTileMultiplierValue(boardTile.multiplier)
                updatedExtendRight(initX, initY, currX, currY + 1, newLetters, newTrie,
                  currPoints + tileScore, crossCheckPoints + crossCheckScore,
                  initialRackSize, tmpBonuses, limitNumber - 1)
              } else if (boardTile.verticalCrossChecks.contains(rackTile.letter) &&
                Option(currTrie.children(rackTile.letter - 65)).nonEmpty) {
                tileScore = TileUtilities.getTileScore(rackTile.letter) *
                  TileUtilities.getTileMultiplierValue(boardTile.multiplier)
                crossCheckScore = boardTile.verticalCrossChecks(rackTile.letter)
                updatedExtendRight(initX, initY, currX, currY + 1, newLetters, newTrie,
                  currPoints + tileScore, crossCheckPoints + crossCheckScore,
                  initialRackSize, tmpBonuses, limitNumber - 1)
              }
            }
          } else if (Option(currTrie.children(boardTile.tile.get.letter - 65)).nonEmpty) {
            val newTrie: Trie = currTrie.children(boardTile.tile.get.letter - 65)
            val tileScore: Int = TileUtilities.getTileScore(boardTile.tile.get.letter)
            updatedExtendRight(initX, initY, currX, currY + 1, letters, newTrie,
              currPoints + tileScore, crossCheckPoints, initialRackSize, tmpBonuses, limitNumber - 1)
          }
        }
      }
    }

    /*
    Extend right potential criteria:-
      - Make sure limit number is reached to ensure back at the original tile
      - Trie must not be null
    Extend right scenarios:-
      - Tile is empty
        - Place tiles that continue trie to right, ensuring that the tile fulfills vertical crossChecks
        - Move on until
      - Tile is not empty
        - Continue trie on to next tile
     */
    @tailrec
    def extendAbove(initX: Int, initY: Int, currX: Int, currY: Int, letters: ListBuffer[PlayerTile],
                    limit: Int, distanceFromOrigin: Int, currTrie: Trie, currPoints: Int,
                    isStartingWord: Boolean): Unit = {

      updatedExtendBelow(initX, initY, currX, currY, letters, currTrie, currPoints,
        0, letters.length, new ListBuffer[Multiplier.Value], distanceFromOrigin)
      if (limit > 0) {
        extendAbove(initX - 1, initY, currX - 1, currY, letters, limit - 1, distanceFromOrigin + 1,
          currTrie, currPoints, isStartingWord)
      }
    }

    def updatedExtendBelow(initX: Int, initY: Int, currX: Int, currY: Int, letters: ListBuffer[PlayerTile],
                           currTrie: Trie, currPoints: Int, crossCheckPoints: Int, initialRackSize: Int,
                           bonuses: ListBuffer[Multiplier.Value], limitNumber: Int): Unit = {
      if (Option(currTrie).nonEmpty) {
        val totalPoints = currentPoints(currPoints, bonuses) + crossCheckPoints +
          (if (initialRackSize == 7 && letters.isEmpty) 50 else 0)
        var tmpBonuses: ListBuffer[Multiplier.Value] = bonuses.clone()

        if (currX == board.boardTiles.length) {
          if (currTrie.isComplete && totalPoints > highestScoringWord.score) {
            setHighestScoringWord(highestScoringWord, currTrie.completedWord, totalPoints, crossCheckPoints,
              initX, initY, Direction.VERTICAL)
          }
        } else {
          val boardTile: BoardTile = board.boardTiles(currX)(currY)

          if (boardTile.tile.isEmpty) {
            if (limitNumber < 0 && letters.length < initialRackSize &&
              currTrie.isComplete &&
              totalPoints > highestScoringWord.score) {
              setHighestScoringWord(highestScoringWord, currTrie.completedWord, totalPoints, crossCheckPoints,
                initX, initY, Direction.VERTICAL)
            }

            boardTile.multiplier match {
              case Multiplier.DOUBLE_WORD => tmpBonuses += boardTile.multiplier
              case Multiplier.TRIPLE_WORD => tmpBonuses += boardTile.multiplier
              case _ =>
            }

            for (tileIndex <- letters.indices) {
              val rackTile: PlayerTile = letters(tileIndex)
              var tileScore: Int = 0
              var crossCheckScore: Int = 0

              val newTrie: Trie = currTrie.children(rackTile.letter - 65)
              val newLetters = letters.clone()

              newLetters.remove(tileIndex)

              if (!boardTile.requiresLeftCrossCheck && !boardTile.requiresRightCrossCheck) {
                tileScore = TileUtilities.getTileScore(rackTile.letter) *
                  TileUtilities.getTileMultiplierValue(boardTile.multiplier)
                updatedExtendBelow(initX, initY, currX + 1, currY, newLetters, newTrie,
                  currPoints + tileScore, crossCheckPoints + crossCheckScore,
                  initialRackSize, tmpBonuses, limitNumber - 1)
              } else if (boardTile.horizontalCrossChecks.contains(rackTile.letter) &&
                Option(currTrie.children(rackTile.letter - 65)).nonEmpty) {
                tileScore = TileUtilities.getTileScore(rackTile.letter) *
                  TileUtilities.getTileMultiplierValue(boardTile.multiplier)
                crossCheckScore = boardTile.horizontalCrossChecks(rackTile.letter)
                updatedExtendBelow(initX, initY, currX + 1, currY, newLetters, newTrie,
                  currPoints + tileScore, crossCheckPoints + crossCheckScore,
                  initialRackSize, tmpBonuses, limitNumber - 1)
              }
            }
          } else if (Option(currTrie.children(boardTile.tile.get.letter - 65)).nonEmpty) {
            val newTrie: Trie = currTrie.children(boardTile.tile.get.letter - 65)
            val tileScore: Int = TileUtilities.getTileScore(boardTile.tile.get.letter)
            updatedExtendBelow(initX, initY, currX + 1, currY, letters, newTrie,
              currPoints + tileScore, crossCheckPoints, initialRackSize, tmpBonuses, limitNumber - 1)
          }
        }
      }
    }

    highestScoringWord
  }


  def currentPoints(score: Int, bonuses: ListBuffer[Multiplier.Value]): Int = {
    score * bonuses.map(v => TileUtilities.getWordMultiplierValue(v)).product
  }

  def setHighestScoringWord(highestScoringWord: HighestScoringWord, completedWord: String,
                            score: Int, crossCheckPoints: Int, initX: Int, initY: Int,
                            direction: Direction.Value): Unit = {
    highestScoringWord.word = completedWord
    highestScoringWord.score = score
    highestScoringWord.crossCheckScore = crossCheckPoints
    highestScoringWord.x = initX
    highestScoringWord.y = initY
    highestScoringWord.direction = direction
  }

  def placeHighestScoringWord(highestScoringWord: HighestScoringWord): Unit = {
    println("Placing " + highestScoringWord.direction + " word: " + highestScoringWord.word +
      " at: " + highestScoringWord.x + "," + highestScoringWord.y + " for " + highestScoringWord.score)
    val newTiles: ListBuffer[PlayerTile] = rack.tiles.clone()

    if (highestScoringWord.direction.equals(Direction.HORIZONTAL)) {
      for (y <- highestScoringWord.y until highestScoringWord.y + highestScoringWord.word.length) {
        if (board.boardTiles(highestScoringWord.x)(y).tile.isEmpty) {
          val char: Char = highestScoringWord.word.charAt(y - highestScoringWord.y)
          val tileIndex: Int = newTiles.indexWhere(playerTile => playerTile.letter.equals(char))
          board.boardTiles(highestScoringWord.x)(y) = FilledBoardTile(newTiles(tileIndex))
          newTiles.remove(tileIndex)
        }
      }
    } else {
      for (x <- highestScoringWord.x until highestScoringWord.x + highestScoringWord.word.length) {
        if (board.boardTiles(x)(highestScoringWord.y).tile.isEmpty) {
          val char: Char = highestScoringWord.word.charAt(x - highestScoringWord.x)
          val tileIndex: Int = newTiles.indexWhere(playerTile => playerTile.letter.equals(char))
          board.boardTiles(x)(highestScoringWord.y) = FilledBoardTile(newTiles(tileIndex))
          newTiles.remove(tileIndex)
        }
      }
    }
    rack.setRack(newTiles)
  }

  def hasJoinedToWord(boardTile: BoardTile): Boolean = {
    boardTile.requiresLeftCrossCheck || boardTile.requiresRightCrossCheck ||
      boardTile.requiresAboveCrossCheck || boardTile.requiresBelowCrossCheck
  }

  def printBoard(): Unit = {
    println("-------------------------------------------")
    board.boardTiles.foreach(row => printBoardRow(row))
    println("-------------------------------------------")
  }

  def printBoardRow(boardTile: Array[BoardTile]): Unit = {
    println(boardTile.map(tile => Console.BLUE + "|" +
      (if (tile.tile.nonEmpty) tile.tile.get.letter else ' ')
      + "|").mkString(" "))
  }
}
