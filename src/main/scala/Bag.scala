import scala.collection.mutable.ListBuffer
import scala.util.Random

class Bag(var tiles: ListBuffer[PlayerTile]) {
  def drawTiles(numberOfTiles: Int): List[PlayerTile] = {
    if (tiles.isEmpty) List.empty
    else {
      var list: ListBuffer[PlayerTile] = new ListBuffer[PlayerTile]()
      val tilesToDraw: Int = if (numberOfTiles > tiles.length) tiles.length else numberOfTiles
      for (_ <- 0 until tilesToDraw) {
        val index: Int = Random.nextInt(tiles.size)
        val tile: PlayerTile = tiles.remove(index)
        list += tile
      }
      list.toList
    }
  }
}
