import TileUtilities.Blank

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object BoardUtilities {

  def findPossibleScoringWords(board: Board, trie: Trie, rack: Rack,
                               isStartingWord: Boolean): mutable.HashSet[HighestScoringWord] = {
    val highestScoringWords: mutable.HashSet[HighestScoringWord] = new mutable.HashSet[HighestScoringWord]()

    for (x <- board.boardTiles.indices) {
      for (y <- board.boardTiles(x).indices) {
        val boardTile: BoardTile = board.boardTiles(x)(y)

        if (boardTile.isAnchor) {

          // Horizontal words
          if (boardTile.requiresLeftCrossCheck && boardTile.horizontalCrossChecks.nonEmpty) {
            val startingPoint = findStartingLeftHorizontalNonAnchorTile(board, x, y - 1)
            val startingHorizontal = updateHorizontalStartingTrie(board, x, startingPoint, y, trie, 0)
            extendRight(x, startingPoint, x, y, rack.tiles, startingHorizontal._1, startingHorizontal._2,
              0, rack.tiles.length, new ListBuffer[Multiplier.Value], 0)
          } else if (!boardTile.requiresLeftCrossCheck) {
            val horizontalLimit: Int = findHorizontalLimit(board, rack, x, y, 0)
            extendLeft(x, y, x, y, rack.tiles, horizontalLimit, 0, trie,
              0, isStartingWord)
          }

          // Vertical words
          if (boardTile.requiresAboveCrossCheck && boardTile.verticalCrossChecks.nonEmpty) {
            val startingPoint = findStartingAboveVerticalNonAnchorTile(board, x - 1, y)
            val startingVertical = updateVerticalStartingTrie(board, startingPoint, y, x, trie, 0)
            extendBelow(startingPoint, y, x, y, rack.tiles, startingVertical._1, startingVertical._2,
              0, rack.tiles.length, new ListBuffer[Multiplier.Value], 0)
          } else if (!boardTile.requiresAboveCrossCheck) {
            val verticalLimit: Int = findVerticalLimit(board, rack, x, y, 0)
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
          if (currTrie.isComplete) {
            highestScoringWords.addOne(createHighestScoringWord(
              currTrie, initX, initY, totalPoints, crossCheckPoints, Direction.HORIZONTAL))
          }
        } else {
          val boardTile: BoardTile = board.boardTiles(currX)(currY)

          if (boardTile.tile.isEmpty) {
            if (limitNumber < 0 && letters.length < initialRackSize &&
              currTrie.isComplete) {
              highestScoringWords.addOne(createHighestScoringWord(
                currTrie, initX, initY, totalPoints, crossCheckPoints, Direction.HORIZONTAL))
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
          if (currTrie.isComplete) {
            highestScoringWords.addOne(createHighestScoringWord(
              currTrie, initX, initY, totalPoints, crossCheckPoints, Direction.VERTICAL))
          }
        } else {
          val boardTile: BoardTile = board.boardTiles(currX)(currY)

          if (boardTile.tile.isEmpty) {
            if (limitNumber < 0 && letters.length < initialRackSize && currTrie.isComplete) {
              highestScoringWords.addOne(createHighestScoringWord(
                currTrie, initX, initY, totalPoints, crossCheckPoints, Direction.VERTICAL))
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

    highestScoringWords
  }

  def currentPoints(score: Int, bonuses: ListBuffer[Multiplier.Value]): Int = {
    score * bonuses.map(v => TileUtilities.getWordMultiplierValue(v)).product
  }

  @tailrec
  final def findStartingLeftHorizontalNonAnchorTile(board: Board, x: Int, y: Int): Int = (x, y) match {
    case (x, y) if board.boardTiles(x)(y).isAnchor => y + 1
    case (_, 0) => 0
    case (x, y) => findStartingLeftHorizontalNonAnchorTile(board, x, y - 1)
  }

  @tailrec
  final def updateHorizontalStartingTrie(board: Board, x: Int, y: Int, targetY: Int,
                                         tmpTrie: Trie, wordPoints: Int): (Trie, Int) = {
    if (y == targetY) (tmpTrie, wordPoints)
    else {
      updateHorizontalStartingTrie(board, x, y + 1, targetY,
        tmpTrie.children(board.boardTiles(x)(y).tile.get.letter - 65),
        wordPoints + TileUtilities.getTileScore(board.boardTiles(x)(y).tile.get.letter))
    }
  }

  @tailrec
  final def findHorizontalLimit(board: Board, rack: Rack, x: Int, y: Int,
                                limit: Int): Int = (x, y, limit) match {
    case (_, 0, _) => limit
    case (_, _, limit) if limit == rack.tiles.length - 1 => limit
    case (x, y, limit) if board.boardTiles(x)(y - 1).isAnchor => limit
    case (x, y, limit) => findHorizontalLimit(board, rack, x, y - 1, limit + 1)
  }

  @tailrec
  final def findStartingAboveVerticalNonAnchorTile(board: Board, x: Int, y: Int): Int = (x, y) match {
    case (x, y) if board.boardTiles(x)(y).isAnchor => x + 1
    case (0, _) => 0
    case (x, y) => findStartingAboveVerticalNonAnchorTile(board, x - 1, y)
  }

  @tailrec
  final def updateVerticalStartingTrie(board: Board, x: Int, y: Int, targetX: Int, tmpTrie: Trie,
                                       wordPoints: Int): (Trie, Int) = {
    if (x == targetX) (tmpTrie, wordPoints)
    else {
      updateVerticalStartingTrie(board, x + 1, y, targetX,
        tmpTrie.children(board.boardTiles(x)(y).tile.get.letter - 65),
        wordPoints + TileUtilities.getTileScore(board.boardTiles(x)(y).tile.get.letter))
    }
  }

  @tailrec
  final def findVerticalLimit(board: Board, rack: Rack, x: Int, y: Int, limit: Int): Int = (x, y, limit) match {
    case (0, _, _) => limit
    case (_, _, limit) if limit == rack.tiles.length - 1 => limit
    case (x, y, limit) if board.boardTiles(x - 1)(y).isAnchor => limit
    case (x, y, limit) => findVerticalLimit(board, rack, x - 1, y, limit + 1)
  }

  final def createHighestScoringWord(trie: Trie, initX: Int, initY: Int, totalPoints: Int,
                                     crossCheckPoints: Int, direction: Direction.Value): HighestScoringWord = {
    new HighestScoringWord(trie.completedWord, initX, initY, totalPoints, crossCheckPoints, ListBuffer.empty, direction)
  }
}
