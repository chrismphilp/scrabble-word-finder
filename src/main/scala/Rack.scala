import scala.collection.mutable.ListBuffer

class Rack(var tiles: ListBuffer[Tile]) {
  def fillRack(bag: Bag): Unit = tiles ++= bag.drawTiles(7 - tiles.length)
}
