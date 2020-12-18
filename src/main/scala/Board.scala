class Board(var boardTiles: Array[Array[BoardTile]]) {

  def updateCrossChecks(): Unit = {
    for (x <- boardTiles.indices) {
      for (y <- boardTiles(x).indices) {
        val boardTile: BoardTile = boardTiles(x)(y)
        if (boardTile.isAnchor) {

        }
      }
    }
  }

  def updateAnchorTiles(): Unit = {
    for (x <- boardTiles.indices) {
      for (y <- boardTiles(x).indices) {
        val boardTile: BoardTile = boardTiles(x)(y)
        if (boardTile.tile.isEmpty) {
          boardTile.isAnchor = checkSurroundingTiles(x, y)
        } else boardTile.isAnchor = false
      }
    }
  }

  def checkSurroundingTiles(x: Int, y: Int): Boolean = {
    val LEFT: Int = x - 1
    val RIGHT: Int = x + 1
    val ABOVE: Int = y + 1
    val BELOW: Int = y - 1

    (ABOVE >= 0 && ABOVE < boardTiles.length && boardTiles(x)(ABOVE).tile.nonEmpty) ||
      (BELOW >= 0 && BELOW < boardTiles.length && boardTiles(x)(BELOW).tile.nonEmpty) ||
      (LEFT >= 0 && LEFT < boardTiles.length && boardTiles(LEFT)(y).tile.nonEmpty) ||
      (RIGHT >= 0 && RIGHT < boardTiles.length && boardTiles(RIGHT)(y).tile.nonEmpty)
  }
}
