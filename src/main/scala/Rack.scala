import scala.collection.mutable.ListBuffer

class Rack(var tiles: ListBuffer[PlayerTile]) {
  def fillRack(bag: Bag): Unit = tiles ++= bag.drawTiles(7 - tiles.length)

  def setRack(tiles: ListBuffer[PlayerTile]): Unit = this.tiles = tiles

  def printRack(): Unit = "Rack tiles: " + println(tiles.map(tile => tile.letter).mkString(","))
}
