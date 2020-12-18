import scala.collection.mutable.ListBuffer
import scala.util.Random

class Bag(var tiles: ListBuffer[PlayerTile]) {
  def drawTiles(numberOfTiles: Int): List[PlayerTile] = {
    if (tiles.isEmpty) List.empty
    else {
      var list: ListBuffer[PlayerTile] = new ListBuffer[PlayerTile]()
      for (_ <- 0 until numberOfTiles) {
        val index: Int = Random.nextInt(tiles.size)
        val tile: PlayerTile = tiles.remove(index)
        list += tile
      }
      list.toList
    }
  }
}
