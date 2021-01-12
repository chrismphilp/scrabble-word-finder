import scala.annotation.tailrec
import scala.collection.mutable

class Board(var boardTiles: Array[Array[BoardTile]], val trie: Trie) {

  def updateBoard(): Unit = {
    for (x <- boardTiles.indices) {
      for (y <- boardTiles(x).indices) {
        resetTile(x, y)
        updateAnchorTile(x, y)
        updateCrossCheckTile(x, y)
      }
    }
  }

  def resetTile(x: Int, y: Int): Unit = {
    val boardTile: BoardTile = boardTiles(x)(y)
    boardTile.isAnchor = false
    boardTile.requiresAboveCrossCheck = false
    boardTile.requiresBelowCrossCheck = false
    boardTile.requiresRightCrossCheck = false
    boardTile.requiresLeftCrossCheck = false
    boardTile.horizontalCrossChecks = mutable.HashMap.empty
    boardTile.verticalCrossChecks = mutable.HashMap.empty
  }

  def updateAnchorTile(x: Int, y: Int): Unit = {
    val boardTile: BoardTile = boardTiles(x)(y)

    if (boardTile.tile.isEmpty) {
      boardTiles(x)(y).requiresAboveCrossCheck = isAboveCrossCheckRequired(x, y)
      boardTile.requiresBelowCrossCheck = isBelowCrossCheckRequired(x, y)
      boardTile.requiresRightCrossCheck = isRightCrossCheckRequired(x, y)
      boardTile.requiresLeftCrossCheck = isLeftCrossCheckRequired(x, y)
      boardTile.isAnchor = boardTile.requiresLeftCrossCheck || boardTile.requiresRightCrossCheck ||
        boardTile.requiresAboveCrossCheck || boardTile.requiresBelowCrossCheck
    }
  }

  def isAboveCrossCheckRequired(x: Int, y: Int): Boolean = {
    val ABOVE: Int = x - 1
    ABOVE >= 0 && boardTiles(ABOVE)(y).tile.nonEmpty
  }

  def isBelowCrossCheckRequired(x: Int, y: Int): Boolean = {
    val BELOW: Int = x + 1
    BELOW < boardTiles.length && boardTiles(BELOW)(y).tile.nonEmpty
  }

  def isRightCrossCheckRequired(x: Int, y: Int): Boolean = {
    val RIGHT: Int = y + 1
    RIGHT < boardTiles.length && boardTiles(x)(RIGHT).tile.nonEmpty
  }

  def isLeftCrossCheckRequired(x: Int, y: Int): Boolean = {
    val LEFT: Int = y - 1
    LEFT >= 0 && boardTiles(x)(LEFT).tile.nonEmpty
  }

  def updateCrossCheckTile(x: Int, y: Int): Unit = {
    val boardTile: BoardTile = boardTiles(x)(y)
    if (boardTile.isAnchor) {
      if (boardTile.requiresAboveCrossCheck || boardTile.requiresBelowCrossCheck) {
        updateVerticalCrossChecks(x, y)
      }
      if (boardTile.requiresRightCrossCheck || boardTile.requiresLeftCrossCheck) {
        updateHorizontalCrossChecks(x, y)
      }
    }
  }

  def updateHorizontalCrossChecks(x: Int, y: Int): Unit = {

    var startingYPoint = y
    var startingCrossSumPoints = 0
    var startingTrie: Trie = trie

    // Need to get Trie to correct point
    if (boardTiles(x)(y).requiresLeftCrossCheck) {
      startingYPoint = findStartingHorizontalPoint(x, y - 1)
      val preHorizontal: (Trie, Int) = processHorizontalPrePartOfWord(
        x, startingYPoint, y, startingTrie, 0)
      startingTrie = preHorizontal._1
      startingCrossSumPoints = preHorizontal._2
    }

    if (Option(startingTrie).nonEmpty) {
      var shouldDouble: Boolean = false
      var shouldTriple: Boolean = false

      boardTiles(x)(y).multiplier match {
        case Multiplier.DOUBLE_WORD => shouldDouble = true
        case Multiplier.TRIPLE_WORD => shouldTriple = true
        case _ =>
      }

      // Skip over blank tile
      startingYPoint = y + 1

      for (i <- 0 until 26) {
        var tmpTrie = startingTrie.children(i)

        if (Option(tmpTrie).nonEmpty) {
          var currY = startingYPoint

          var currPoints = TileUtilities.getTileScore(tmpTrie.value) *
            (boardTiles(x)(y).multiplier match {
              case Multiplier.DOUBLE_LETTER => 2
              case Multiplier.TRIPLE_LETTER => 3
              case _ => 1
            }) + startingCrossSumPoints

          val postHorizontal: (Int, Trie, Int) = processHorizontalPostPartOfWord(x, currY,
            tmpTrie, currPoints)

          currY = postHorizontal._1
          tmpTrie = postHorizontal._2
          currPoints = postHorizontal._3

          if ((currY == boardTiles.length || boardTiles(x)(currY).tile.isEmpty) && tmpTrie.isComplete) {
            addHorizontalCrossChecks(x, y, currPoints, (i + 65).toChar, shouldDouble, shouldTriple)
          }
        }
      }
    }
  }

  @tailrec
  final def findStartingHorizontalPoint(x: Int, y: Int): Int = (x, y) match {
    case (x, y) if boardTiles(x)(y).tile.isEmpty => y + 1
    case (_, 0) => 0
    case (_, y) => findStartingHorizontalPoint(x, y - 1)
  }

  @tailrec
  final def processHorizontalPrePartOfWord(x: Int, y: Int, target: Int,
                                           tmpTrie: Trie, crossSumPoints: Int): (Trie, Int) = {
    if (y == target) (tmpTrie, crossSumPoints)
    else processHorizontalPrePartOfWord(x, y + 1, target,
      tmpTrie.children(boardTiles(x)(y).tile.get.letter - 65),
      crossSumPoints + TileUtilities.getTileScore(boardTiles(x)(y).tile.get.letter))
  }

  @tailrec
  final def processHorizontalPostPartOfWord(x: Int, y: Int, tmpTrie: Trie,
                                            points: Int): (Int, Trie, Int) = {
    if (y < boardTiles.length &&
      boardTiles(x)(y).tile.nonEmpty &&
      Option(tmpTrie.children(boardTiles(x)(y).tile.get.letter - 65)).nonEmpty) {
      processHorizontalPostPartOfWord(x, y + 1,
        tmpTrie.children(boardTiles(x)(y).tile.get.letter - 65),
        points + TileUtilities.getTileScore(boardTiles(x)(y).tile.get.letter))
    } else (y, tmpTrie, points)
  }

  def addHorizontalCrossChecks(x: Int, y: Int, points: Int, char: Char,
                               shouldDouble: Boolean, shouldTriple: Boolean): Unit = {
    var currPoints: Int = points
    if (!boardTiles(x)(y).horizontalCrossChecks.contains(' ')) {
      var withBlankScore: Int = currPoints - TileUtilities.getTileScore(char)
      if (shouldDouble) withBlankScore *= 2
      if (shouldTriple) withBlankScore *= 3
      boardTiles(x)(y).horizontalCrossChecks += ' ' -> withBlankScore
    }
    if (shouldDouble) currPoints *= 2
    if (shouldTriple) currPoints *= 3
    boardTiles(x)(y).horizontalCrossChecks += char -> currPoints
  }

  def updateVerticalCrossChecks(x: Int, y: Int): Unit = {

    var startingXPoint = x
    var startingCrossSumPoints = 0
    var startingTrie: Trie = trie

    // Need to get Trie to correct point
    if (boardTiles(x)(y).requiresAboveCrossCheck) {
      startingXPoint = findStartingVerticalPoint(x - 1, y)
      val preVertical: (Trie, Int) = processVerticalPrePartOfWord(
        startingXPoint, y, x, startingTrie, 0)
      startingTrie = preVertical._1
      startingCrossSumPoints = preVertical._2
    }

    if (Option(startingTrie).nonEmpty) {
      var shouldDouble: Boolean = false
      var shouldTriple: Boolean = false

      boardTiles(x)(y).multiplier match {
        case Multiplier.DOUBLE_WORD => shouldDouble = true
        case Multiplier.TRIPLE_WORD => shouldTriple = true
        case _ =>
      }

      // Skip over blank tile
      startingXPoint = x + 1

      for (i <- 0 until 26) {
        var tmpTrie = startingTrie.children(i)

        if (Option(tmpTrie).nonEmpty) {
          var currX = startingXPoint

          var currPoints = TileUtilities.getTileScore(tmpTrie.value) *
            (boardTiles(x)(y).multiplier match {
              case Multiplier.DOUBLE_LETTER => 2
              case Multiplier.TRIPLE_LETTER => 3
              case _ => 1
            }) + startingCrossSumPoints

          val postVertical: (Int, Trie, Int) = processVerticalPostPartOfWord(currX, y,
            tmpTrie, currPoints)

          currX = postVertical._1
          tmpTrie = postVertical._2
          currPoints = postVertical._3

          if ((currX == boardTiles.length || boardTiles(currX)(y).tile.isEmpty) && tmpTrie.isComplete) {
            addVerticalCrossChecks(x, y, currPoints, (i + 65).toChar, shouldDouble, shouldTriple)
          }
        }
      }
    }
  }

  @tailrec
  final def findStartingVerticalPoint(x: Int, y: Int): Int = (x, y) match {
    case (x, y) if boardTiles(x)(y).tile.isEmpty => x + 1
    case (0, _) => 0
    case (x, y) => findStartingVerticalPoint(x - 1, y)
  }

  @tailrec
  final def processVerticalPrePartOfWord(x: Int, y: Int, target: Int,
                                         tmpTrie: Trie, crossSumPoints: Int): (Trie, Int) = {
    if (x == target) (tmpTrie, crossSumPoints)
    else processVerticalPrePartOfWord(x + 1, y, target,
      tmpTrie.children(boardTiles(x)(y).tile.get.letter - 65),
      crossSumPoints + TileUtilities.getTileScore(boardTiles(x)(y).tile.get.letter))
  }

  @tailrec
  final def processVerticalPostPartOfWord(x: Int, y: Int, tmpTrie: Trie,
                                          points: Int): (Int, Trie, Int) = {
    if (x < boardTiles.length &&
      boardTiles(x)(y).tile.nonEmpty &&
      Option(tmpTrie.children(boardTiles(x)(y).tile.get.letter - 65)).nonEmpty) {
      processVerticalPostPartOfWord(x + 1, y,
        tmpTrie.children(boardTiles(x)(y).tile.get.letter - 65),
        points + TileUtilities.getTileScore(boardTiles(x)(y).tile.get.letter))
    } else (x, tmpTrie, points)
  }

  def addVerticalCrossChecks(x: Int, y: Int, points: Int, char: Char,
                             shouldDouble: Boolean, shouldTriple: Boolean): Unit = {
    var currPoints: Int = points
    if (!boardTiles(x)(y).verticalCrossChecks.contains(' ')) {
      var withBlankScore: Int = currPoints - TileUtilities.getTileScore(char)
      if (shouldDouble) withBlankScore *= 2
      if (shouldTriple) withBlankScore *= 3
      boardTiles(x)(y).verticalCrossChecks += ' ' -> withBlankScore
    }
    if (shouldDouble) currPoints *= 2
    if (shouldTriple) currPoints *= 3
    boardTiles(x)(y).verticalCrossChecks += char -> currPoints
  }

  def printBoard(): Unit = {
    println("-------------------------------------------")
    boardTiles.foreach(row => printBoardRow(row))
    println("-------------------------------------------")
  }

  def printBoardRow(boardTile: Array[BoardTile]): Unit = {
    println(boardTile.map(tile => Console.BLUE + "|" +
      (if (tile.tile.nonEmpty) tile.tile.get.letter else ' ')
      + "|").mkString(" "))
  }

  def countFilledBoardTiles(): Int = boardTiles.map(row => row.count(A => A.tile.nonEmpty)).sum
}
