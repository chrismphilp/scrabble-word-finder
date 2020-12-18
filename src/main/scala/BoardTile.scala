import scala.collection.mutable.ListBuffer

class BoardTile(var tile: Option[PlayerTile], val multiplier: Multiplier.Value,
                var crossChecks: ListBuffer[PlayerTile], var isAnchor: Boolean)
