import scala.collection.mutable

class BoardTile(var tile: Option[PlayerTile],
                val multiplier: Multiplier.Value,
                var horizontalCrossChecks: mutable.HashSet[Char],
                var requiresRightCrossCheck: Boolean,
                var requiresLeftCrossCheck: Boolean,
                var verticalCrossChecks: mutable.HashSet[Char],
                var requiresAboveCrossCheck: Boolean,
                var requiresBelowCrossCheck: Boolean,
                var isAnchor: Boolean)
