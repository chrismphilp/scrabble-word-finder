import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class Board(var boardTiles: Array[Array[BoardTile]], val trie: Trie) {

  def updateBoard(): Unit = {
    for (x <- boardTiles.indices) {
      for (y <- boardTiles(x).indices) {
        updateAnchorTile(x, y)
        updateCrossCheckTile(x, y)
      }
    }
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
    } else {
      boardTile.isAnchor = false
      boardTile.requiresAboveCrossCheck = false
      boardTile.requiresBelowCrossCheck = false
      boardTile.requiresRightCrossCheck = false
      boardTile.requiresLeftCrossCheck = false
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
    } else {
      boardTile.horizontalCrossChecks = new mutable.HashSet[Char]
      boardTile.verticalCrossChecks = new mutable.HashSet[Char]
    }
  }

  def updateHorizontalCrossChecks(x: Int, y: Int): Unit = {

    var startingPoint = y
    var startingTrie: Trie = trie

    // Need to get Trie to correct point
    if (boardTiles(x)(y).requiresLeftCrossCheck) {
      startingPoint -= 1
      while (startingPoint > 0 && boardTiles(x)(startingPoint).tile.nonEmpty) startingPoint -= 1
      if (boardTiles(x)(startingPoint).tile.isEmpty) startingPoint += 1

      while (startingPoint != y) {
        startingTrie = startingTrie.children(boardTiles(x)(startingPoint).tile.get.letter - 65)
        startingPoint += 1
      }
    }
    startingPoint += 1

    for (i <- 0 until 26) {
      var tmpTrie = startingTrie.children(i)

      if (tmpTrie != null) {
        var curr = startingPoint

        while (curr < boardTiles.length &&
          boardTiles(x)(curr).tile.nonEmpty &&
          Option(tmpTrie.children(boardTiles(x)(curr).tile.get.letter - 65)).nonEmpty) {
          tmpTrie = tmpTrie.children(boardTiles(x)(curr).tile.get.letter - 65)
          curr += 1
        }

        if ((curr == boardTiles.length || boardTiles(x)(curr).tile.isEmpty) && tmpTrie.isComplete) {
          val char: Char = (i + 65).toChar
          boardTiles(x)(y).horizontalCrossChecks += char
        }
      }
    }
  }

  def updateVerticalCrossChecks(x: Int, y: Int): Unit = {

    var startingPoint = x
    var startingTrie: Trie = trie

    // Need to get Trie to correct point
    if (boardTiles(x)(y).requiresAboveCrossCheck) {
      startingPoint -= 1
      while (startingPoint > 0 && boardTiles(startingPoint)(y).tile.nonEmpty) startingPoint -= 1
      if (boardTiles(startingPoint)(y).tile.isEmpty) startingPoint += 1

      while (startingPoint != x) {
        startingTrie = startingTrie.children(boardTiles(startingPoint)(y).tile.get.letter - 65)
        startingPoint += 1
      }
    }
    startingPoint += 1

    for (i <- 0 until 26) {
      var tmpTrie = startingTrie.children(i)

      if (tmpTrie != null) {
        var curr = startingPoint

        while (curr < boardTiles.length &&
          boardTiles(curr)(y).tile.nonEmpty &&
          Option(tmpTrie.children(boardTiles(curr)(y).tile.get.letter - 65)).nonEmpty) {
          tmpTrie = tmpTrie.children(boardTiles(curr)(y).tile.get.letter - 65)
          curr += 1
        }

        if ((curr == boardTiles.length || boardTiles(curr)(y).tile.isEmpty) && tmpTrie.isComplete) {
          val char: Char = (i + 65).toChar
          boardTiles(x)(y).verticalCrossChecks += char
        }
      }
    }
  }
}
