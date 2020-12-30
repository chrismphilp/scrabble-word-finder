import TileUtilities.{Blank, FilledBoardTile}

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

  @tailrec
  final def findStartingLeftHorizontalNonAnchorTile(x: Int, y: Int): Int = (x, y) match {
    case (x, y) if board.boardTiles(x)(y).isAnchor => y + 1
    case (_, 0) => 0
    case (x, y) => findStartingLeftHorizontalNonAnchorTile(x, y - 1)
  }

  @tailrec
  final def updateHorizontalStartingTrie(x: Int, y: Int, targetY: Int, tmpTrie: Trie,
                                         wordPoints: Int): (Trie, Int) = {
    if (y == targetY) (tmpTrie, wordPoints)
    else {
      updateHorizontalStartingTrie(x, y + 1, targetY,
        tmpTrie.children(board.boardTiles(x)(y).tile.get.letter - 65),
        wordPoints + TileUtilities.getTileScore(board.boardTiles(x)(y).tile.get.letter))
    }
  }

  @tailrec
  final def findHorizontalLimit(x: Int, y: Int, limit: Int): Int = (x, y, limit) match {
    case (_, 0, _) => limit
    case (_, _, limit) if limit == rack.tiles.length - 1 => limit
    case (x, y, limit) if board.boardTiles(x)(y - 1).isAnchor => limit
    case (x, y, limit) => findHorizontalLimit(x, y - 1, limit + 1)
  }

  @tailrec
  final def findStartingAboveVerticalNonAnchorTile(x: Int, y: Int): Int = (x, y) match {
    case (x, y) if board.boardTiles(x)(y).isAnchor => x + 1
    case (0, _) => 0
    case (x, y) => findStartingAboveVerticalNonAnchorTile(x - 1, y)
  }

  @tailrec
  final def updateVerticalStartingTrie(x: Int, y: Int, targetX: Int, tmpTrie: Trie,
                                       wordPoints: Int): (Trie, Int) = {
    if (x == targetX) (tmpTrie, wordPoints)
    else {
      updateVerticalStartingTrie(x + 1, y, targetX,
        tmpTrie.children(board.boardTiles(x)(y).tile.get.letter - 65),
        wordPoints + TileUtilities.getTileScore(board.boardTiles(x)(y).tile.get.letter))
    }
  }

  @tailrec
  final def findVerticalLimit(x: Int, y: Int, limit: Int): Int = (x, y, limit) match {
    case (0, _, _) => limit
    case (_, _, limit) if limit == rack.tiles.length - 1 => limit
    case (x, y, limit) if board.boardTiles(x - 1)(y).isAnchor => limit
    case (x, y, limit) => findVerticalLimit(x - 1, y, limit + 1)
  }

  def findHighestScoringWord(isStartingWord: Boolean): HighestScoringWord = {
    val highestScoringWord: HighestScoringWord =
      new HighestScoringWord("", 0, 0, 0, 0, Direction.HORIZONTAL)

    for (x <- board.boardTiles.indices) {
      for (y <- board.boardTiles(x).indices) {
        val boardTile: BoardTile = board.boardTiles(x)(y)

        if (boardTile.isAnchor) {

          // Horizontal words
          if (boardTile.requiresLeftCrossCheck && boardTile.horizontalCrossChecks.nonEmpty) {
            val startingPoint = findStartingLeftHorizontalNonAnchorTile(x, y - 1)
            val startingHorizontal = updateHorizontalStartingTrie(x, startingPoint, y, trie, 0)
            extendRight(x, startingPoint, x, y, rack.tiles, startingHorizontal._1, startingHorizontal._2,
              0, rack.tiles.length, new ListBuffer[Multiplier.Value], 0)
          } else if (!boardTile.requiresLeftCrossCheck) {
            val horizontalLimit: Int = findHorizontalLimit(x, y, 0)
            extendLeft(x, y, x, y, rack.tiles, horizontalLimit, 0, trie,
              0, isStartingWord)
          }

          // Vertical words
          if (boardTile.requiresAboveCrossCheck && boardTile.verticalCrossChecks.nonEmpty) {
            val startingPoint = findStartingAboveVerticalNonAnchorTile(x - 1, y)
            val startingVertical = updateVerticalStartingTrie(startingPoint, y, x, trie, 0)
            extendBelow(startingPoint, y, x, y, rack.tiles, startingVertical._1, startingVertical._2,
              0, rack.tiles.length, new ListBuffer[Multiplier.Value], 0)
          } else if (!boardTile.requiresAboveCrossCheck) {
            val verticalLimit: Int = findVerticalLimit(x, y, 0)
            extendAbove(x, y, x, y, rack.tiles, verticalLimit, 0,
              trie, 0, isStartingWord)
          }
        }
      }
    }

    @tailrec
    def extendLeft(initX: Int, initY: Int, currX: Int, currY: Int, letters: ListBuffer[PlayerTile],
                   limit: Int, distanceFromOrigin: Int, currTrie: Trie, currPoints: Int,
                   isStartingWord: Boolean): Unit = {

      extendRight(initX, initY, currX, currY, letters, currTrie, currPoints,
        0, letters.length, new ListBuffer[Multiplier.Value],
        if (isStartingWord) 0 else distanceFromOrigin)
      if (limit > 0) {
        extendLeft(initX, initY - 1, currX, currY - 1, letters, limit - 1, distanceFromOrigin + 1,
          currTrie, currPoints, isStartingWord)
      }
    }

    def extendRight(initX: Int, initY: Int, currX: Int, currY: Int, letters: ListBuffer[PlayerTile],
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
              val newLetters = letters.clone()
              newLetters.remove(tileIndex)

              if (rackTile.letter.equals(Blank().letter)) {
                for (i <- 0 until 26) {
                  val blankChar: Char = (65 + i).toChar
                  val newTrie: Trie = currTrie.children(i)

                  if (Option(newTrie).nonEmpty) {
                    if (!boardTile.requiresAboveCrossCheck && !boardTile.requiresBelowCrossCheck) {
                      extendRight(initX, initY, currX, currY + 1, newLetters, newTrie,
                        currPoints + tileScore, crossCheckPoints + crossCheckScore,
                        initialRackSize, tmpBonuses, limitNumber - 1)
                    } else if (boardTile.verticalCrossChecks.contains(blankChar)) {
                      crossCheckScore = boardTile.verticalCrossChecks(rackTile.letter)
                      extendRight(initX, initY, currX, currY + 1, newLetters, newTrie,
                        currPoints + tileScore, crossCheckPoints + crossCheckScore,
                        initialRackSize, tmpBonuses, limitNumber - 1)
                    }
                  }
                }
              } else {
                val newTrie: Trie = currTrie.children(rackTile.letter - 65)

                if (!boardTile.requiresAboveCrossCheck && !boardTile.requiresBelowCrossCheck) {
                  tileScore = TileUtilities.getTileScore(rackTile.letter) *
                    TileUtilities.getTileMultiplierValue(boardTile.multiplier)
                  extendRight(initX, initY, currX, currY + 1, newLetters, newTrie,
                    currPoints + tileScore, crossCheckPoints + crossCheckScore,
                    initialRackSize, tmpBonuses, limitNumber - 1)
                } else if (boardTile.verticalCrossChecks.contains(rackTile.letter)) {
                  tileScore = TileUtilities.getTileScore(rackTile.letter) *
                    TileUtilities.getTileMultiplierValue(boardTile.multiplier)
                  crossCheckScore = boardTile.verticalCrossChecks(rackTile.letter)
                  extendRight(initX, initY, currX, currY + 1, newLetters, newTrie,
                    currPoints + tileScore, crossCheckPoints + crossCheckScore,
                    initialRackSize, tmpBonuses, limitNumber - 1)
                }
              }
            }
          } else if (Option(currTrie.children(boardTile.tile.get.letter - 65)).nonEmpty) {
            val newTrie: Trie = currTrie.children(boardTile.tile.get.letter - 65)
            val tileScore: Int = TileUtilities.getTileScore(boardTile.tile.get.letter)
            extendRight(initX, initY, currX, currY + 1, letters, newTrie,
              currPoints + tileScore, crossCheckPoints, initialRackSize, tmpBonuses, limitNumber - 1)
          }
        }
      }
    }

    @tailrec
    def extendAbove(initX: Int, initY: Int, currX: Int, currY: Int, letters: ListBuffer[PlayerTile],
                    limit: Int, distanceFromOrigin: Int, currTrie: Trie, currPoints: Int,
                    isStartingWord: Boolean): Unit = {

      extendBelow(initX, initY, currX, currY, letters, currTrie, currPoints,
        0, letters.length, new ListBuffer[Multiplier.Value], distanceFromOrigin)
      if (limit > 0) {
        extendAbove(initX - 1, initY, currX - 1, currY, letters, limit - 1, distanceFromOrigin + 1,
          currTrie, currPoints, isStartingWord)
      }
    }

    def extendBelow(initX: Int, initY: Int, currX: Int, currY: Int, letters: ListBuffer[PlayerTile],
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
              val newLetters = letters.clone()
              newLetters.remove(tileIndex)

              if (rackTile.letter.equals(Blank().letter)) {
                for (i <- 0 until 26) {
                  val blankChar: Char = (65 + i).toChar
                  val newTrie: Trie = currTrie.children(i)

                  if (Option(newTrie).nonEmpty) {
                    if (!boardTile.requiresLeftCrossCheck && !boardTile.requiresRightCrossCheck) {
                      extendBelow(initX, initY, currX + 1, currY, newLetters, newTrie,
                        currPoints + tileScore, crossCheckPoints + crossCheckScore,
                        initialRackSize, tmpBonuses, limitNumber - 1)
                    } else if (boardTile.horizontalCrossChecks.contains(blankChar)) {
                      crossCheckScore = boardTile.horizontalCrossChecks(rackTile.letter)
                      extendBelow(initX, initY, currX + 1, currY, newLetters, newTrie,
                        currPoints + tileScore, crossCheckPoints + crossCheckScore,
                        initialRackSize, tmpBonuses, limitNumber - 1)
                    }
                  }
                }
              } else {
                val newTrie: Trie = currTrie.children(rackTile.letter - 65)

                if (!boardTile.requiresLeftCrossCheck && !boardTile.requiresRightCrossCheck) {
                  tileScore = TileUtilities.getTileScore(rackTile.letter) *
                    TileUtilities.getTileMultiplierValue(boardTile.multiplier)
                  extendBelow(initX, initY, currX + 1, currY, newLetters, newTrie,
                    currPoints + tileScore, crossCheckPoints + crossCheckScore,
                    initialRackSize, tmpBonuses, limitNumber - 1)
                } else if (boardTile.horizontalCrossChecks.contains(rackTile.letter)) {
                  tileScore = TileUtilities.getTileScore(rackTile.letter) *
                    TileUtilities.getTileMultiplierValue(boardTile.multiplier)
                  crossCheckScore = boardTile.horizontalCrossChecks(rackTile.letter)
                  extendBelow(initX, initY, currX + 1, currY, newLetters, newTrie,
                    currPoints + tileScore, crossCheckPoints + crossCheckScore,
                    initialRackSize, tmpBonuses, limitNumber - 1)
                }
              }
            }
          } else if (Option(currTrie.children(boardTile.tile.get.letter - 65)).nonEmpty) {
            val newTrie: Trie = currTrie.children(boardTile.tile.get.letter - 65)
            val tileScore: Int = TileUtilities.getTileScore(boardTile.tile.get.letter)
            extendBelow(initX, initY, currX + 1, currY, letters, newTrie,
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
          var tileIndex: Int = newTiles.indexWhere(playerTile => playerTile.letter.equals(char))
          if (tileIndex == -1) {
            tileIndex = newTiles.indexWhere(playerTile => playerTile.letter.equals(' '))
            board.boardTiles(highestScoringWord.x)(y) = FilledBoardTile(new PlayerTile(char, 0))
          } else {
            board.boardTiles(highestScoringWord.x)(y) = FilledBoardTile(newTiles(tileIndex))
          }
          newTiles.remove(tileIndex)
        }
      }
    } else {
      for (x <- highestScoringWord.x until highestScoringWord.x + highestScoringWord.word.length) {
        if (board.boardTiles(x)(highestScoringWord.y).tile.isEmpty) {
          val char: Char = highestScoringWord.word.charAt(x - highestScoringWord.x)
          var tileIndex: Int = newTiles.indexWhere(playerTile => playerTile.letter.equals(char))
          if (tileIndex == -1) {
            tileIndex = newTiles.indexWhere(playerTile => playerTile.letter.equals(' '))
            board.boardTiles(x)(highestScoringWord.y) = FilledBoardTile(new PlayerTile(char, 0))
          } else {
            board.boardTiles(x)(highestScoringWord.y) = FilledBoardTile(newTiles(tileIndex))
          }
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
