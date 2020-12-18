import scala.collection.mutable.ListBuffer
import scala.util.Random

class Bag(var tiles: ListBuffer[Tile]) {
  def drawTiles(numberOfTiles: Int): List[Tile] = {
    if (tiles.isEmpty) List.empty
    else {
      var list: ListBuffer[Tile] = new ListBuffer[Tile]()
      for (_ <- 0 until numberOfTiles) {
        val index: Int = Random.nextInt(tiles.size)
        val tile: Tile = tiles.remove(index)
        list += tile
      }
      list.toList
    }
  }
}
