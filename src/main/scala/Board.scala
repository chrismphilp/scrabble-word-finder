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

    var startingPoint = y
    var startingCrossSumPoints = 0
    var startingTrie: Trie = trie

    // Need to get Trie to correct point
    if (boardTiles(x)(y).requiresLeftCrossCheck) {
      while (startingPoint > 0 && boardTiles(x)(startingPoint - 1).tile.nonEmpty) {
        startingPoint -= 1
      }
      while (startingPoint != y) {
        startingTrie = startingTrie.children(boardTiles(x)(startingPoint).tile.get.letter - 65)
        startingCrossSumPoints += TileUtilities.getTileScore(boardTiles(x)(startingPoint).tile.get.letter)
        startingPoint += 1
      }
    }

    if (Option(startingTrie).nonEmpty) {
      var shouldDouble: Boolean = false
      var shouldTriple: Boolean = false

      boardTiles(x)(startingPoint).multiplier match {
        case Multiplier.DOUBLE_WORD => shouldDouble = true
        case Multiplier.TRIPLE_WORD => shouldTriple = true
        case _ =>
      }

      startingPoint += 1

      for (i <- 0 until 26) {
        var tmpTrie = startingTrie.children(i)

        if (Option(tmpTrie).nonEmpty) {
          var curr = startingPoint

          var currPoints = TileUtilities.getTileScore(tmpTrie.value) *
            (boardTiles(x)(y).multiplier match {
              case Multiplier.DOUBLE_LETTER => 2
              case Multiplier.TRIPLE_LETTER => 3
              case _ => 1
            }) + startingCrossSumPoints

          while (curr < boardTiles.length &&
            boardTiles(x)(curr).tile.nonEmpty &&
            Option(tmpTrie.children(boardTiles(x)(curr).tile.get.letter - 65)).nonEmpty) {
            tmpTrie = tmpTrie.children(boardTiles(x)(curr).tile.get.letter - 65)
            currPoints += TileUtilities.getTileScore(boardTiles(x)(curr).tile.get.letter)
            curr += 1
          }

          if ((curr == boardTiles.length || boardTiles(x)(curr).tile.isEmpty) && tmpTrie.isComplete) {
            val char: Char = (i + 65).toChar
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
        }
      }
    }
  }

  def updateVerticalCrossChecks(x: Int, y: Int): Unit = {

    var startingPoint = x
    var startingCrossSumPoints = 0
    var startingTrie: Trie = trie

    // Need to get Trie to correct point
    if (boardTiles(x)(y).requiresAboveCrossCheck) {
      while (startingPoint > 0 && boardTiles(startingPoint - 1)(y).tile.nonEmpty) startingPoint -= 1

      while (startingPoint != x) {
        startingTrie = startingTrie.children(boardTiles(startingPoint)(y).tile.get.letter - 65)
        startingCrossSumPoints += TileUtilities.getTileScore(boardTiles(startingPoint)(y).tile.get.letter)
        startingPoint += 1
      }
    }

    if (Option(startingTrie).nonEmpty) {
      var shouldDouble: Boolean = false
      var shouldTriple: Boolean = false

      boardTiles(startingPoint)(y).multiplier match {
        case Multiplier.DOUBLE_WORD => shouldDouble = true
        case Multiplier.TRIPLE_WORD => shouldTriple = true
        case _ =>
      }

      startingPoint += 1

      for (i <- 0 until 26) {
        var tmpTrie = startingTrie.children(i)

        if (tmpTrie != null) {
          var curr = startingPoint

          var currPoints = TileUtilities.getTileScore(tmpTrie.value) *
            (boardTiles(x)(y).multiplier match {
              case Multiplier.DOUBLE_LETTER => 2
              case Multiplier.TRIPLE_LETTER => 3
              case _ => 1
            }) + startingCrossSumPoints

          while (curr < boardTiles.length &&
            boardTiles(curr)(y).tile.nonEmpty &&
            Option(tmpTrie.children(boardTiles(curr)(y).tile.get.letter - 65)).nonEmpty) {
            tmpTrie = tmpTrie.children(boardTiles(curr)(y).tile.get.letter - 65)
            currPoints += TileUtilities.getTileScore(boardTiles(curr)(y).tile.get.letter)
            curr += 1
          }

          if ((curr == boardTiles.length || boardTiles(curr)(y).tile.isEmpty) && tmpTrie.isComplete) {
            val char: Char = (i + 65).toChar
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
        }
      }
    }
  }
}
