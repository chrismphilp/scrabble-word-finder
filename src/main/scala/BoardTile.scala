import scala.collection.mutable

class BoardTile(var tile: Option[PlayerTile],
                val multiplier: Multiplier.Value,
                var horizontalCrossChecks: mutable.HashMap[Char, Int],
                var requiresRightCrossCheck: Boolean,
                var requiresLeftCrossCheck: Boolean,
                var verticalCrossChecks: mutable.HashMap[Char, Int],
                var requiresAboveCrossCheck: Boolean,
                var requiresBelowCrossCheck: Boolean,
                var isAnchor: Boolean)
