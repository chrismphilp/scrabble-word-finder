import scala.collection.mutable

class Board(var boardTiles: Array[Array[BoardTile]], val trie: Trie) {

  def updateBoard(): Unit = {
    for (x <- boardTiles.indices) {
      for (y <- boardTiles(x).indices) {
        updateTile(x, y)
      }
    }
  }

  def updateTile(x: Int, y: Int): Unit = {
    val boardTile: BoardTile = boardTiles(x)(y)

    if (boardTile.tile.isEmpty) {
      boardTile.requiresAboveCrossCheck = isAboveCrossCheckRequired(x, y)
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
    updateCrossCheckTile(x, y)
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

    var curr = y

    // Need to get Trie to correct point
    if (boardTiles(x)(y).requiresLeftCrossCheck) {
      while (curr > 0 && boardTiles(x)(curr).tile.nonEmpty) curr -= 1
      if (boardTiles(x)(curr).tile.isEmpty) curr += 1
    }

    for (i <- 0 until 26) {
      val tmpTrie: Trie = trie.children(i)
      curr = y + 1
      while (curr < boardTiles.length &&
        boardTiles(x)(curr).tile.nonEmpty &&
        Option(tmpTrie.children(boardTiles(x)(curr).tile.get.letter - 65)).nonEmpty) {
        curr += 1
      }

      if (tmpTrie.isComplete) {
        boardTiles(curr)(y).horizontalCrossChecks += (i + 65).toChar
      }
    }
  }
}
