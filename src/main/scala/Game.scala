import Algorithm.Algorithm
import TileUtilities.FilledBoardTile

import scala.collection.mutable.ListBuffer

/*
  Algorithm steps:

  1) Before move generation, generate all cross-checks for the row's columns
  2) Find all the anchor points for the board, and cross-checks
  3) Find all the cross-sums for each cross-check
  4) F-o- all anchor points (feasible from rack), generate the left part of possible words
  5) For all anchor points (feasible from rack), generate the right part of possible words
  6) Combine letters multipliers as going, and compute word multipliers when end of word is found
 */
class Game(board: Board, trie: Trie, rack: Rack, bag: Bag) {

  def initializeGame(): Unit = {
    rack.fillRack(bag)
    board.boardTiles = GameUtilities.initialiseBoard()
  }

  def updateBoard(): Unit = board.updateBoard()

  def updateRack(): Unit = rack.fillRack(bag)

  def playerMove(algorithm: Algorithm, isStartingWord: Boolean): HighestScoringWord = algorithm match {
    case Algorithm.OPTIMAL =>
      OptimalAlgorithm.findOptimalPlay()
    case Algorithm.GREEDY =>
      if (isStartingWord) updateRack()
      rack.printRack()
      val highestScoringWord = GreedyAlgorithm.findHighestScoringWord(board, trie, rack, isStartingWord)
      placeHighestScoringWord(highestScoringWord)
      updateRack()
      updateBoard()
      board.printBoard()
      highestScoringWord
  }

  def placeHighestScoringWord(highestScoringWord: HighestScoringWord): Unit = {
    println("Placing " + highestScoringWord.direction + " word: " + highestScoringWord.word +
      " at: " + highestScoringWord.x + "," + highestScoringWord.y + " for " + highestScoringWord.score + ". " +
      "Used tiles: " +  highestScoringWord.tilesUsed.map(tile => tile.letter).mkString(","))
    println("Definition: " + WordDefinition.getWordDefinition(highestScoringWord.word))
    val newTiles: ListBuffer[PlayerTile] = rack.tiles.clone()

    if (highestScoringWord.direction.equals(Direction.HORIZONTAL)) {
      for (y <- highestScoringWord.y until highestScoringWord.y + highestScoringWord.word.length) {
        if (board.boardTiles(highestScoringWord.x)(y).tile.isEmpty) {
          val char: Char = highestScoringWord.word.charAt(y - highestScoringWord.y)
          var tileIndex: Int = newTiles.indexWhere(playerTile => playerTile.letter.equals(char))
          if (tileIndex == -1) {
            tileIndex = newTiles.indexWhere(playerTile => playerTile.letter.equals(' '))
            board.boardTiles(highestScoringWord.x)(y) = FilledBoardTile(new PlayerTile(char, 0))
          } else {
            board.boardTiles(highestScoringWord.x)(y) = FilledBoardTile(newTiles(tileIndex))
          }
          newTiles.remove(tileIndex)
        }
      }
    } else {
      for (x <- highestScoringWord.x until highestScoringWord.x + highestScoringWord.word.length) {
        if (board.boardTiles(x)(highestScoringWord.y).tile.isEmpty) {
          val char: Char = highestScoringWord.word.charAt(x - highestScoringWord.x)
          var tileIndex: Int = newTiles.indexWhere(playerTile => playerTile.letter.equals(char))
          if (tileIndex == -1) {
            tileIndex = newTiles.indexWhere(playerTile => playerTile.letter.equals(' '))
            board.boardTiles(x)(highestScoringWord.y) = FilledBoardTile(new PlayerTile(char, 0))
          } else {
            board.boardTiles(x)(highestScoringWord.y) = FilledBoardTile(newTiles(tileIndex))
          }
          newTiles.remove(tileIndex)
        }
      }
    }
    rack.setRack(newTiles)
  }
}
