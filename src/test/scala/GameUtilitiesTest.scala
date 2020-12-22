import GameUtilities.initialiseBoard
import org.scalatest.funsuite.AnyFunSuite

class GameUtilitiesTest extends AnyFunSuite {
  test("Should correctly initialise board") {
    val initialisedBoard: Array[Array[BoardTile]] = initialiseBoard()
    for (x <- initialisedBoard.indices) {
      initialisedBoard(x).view
        .zip(initialisedBoard(x).reverse.view)
        .forall({
          case (a, b) =>
            a.multiplier == b.multiplier &&
              a.tile.isEmpty == b.tile.isEmpty
        })
    }
  }
}
