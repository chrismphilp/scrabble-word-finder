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
class Game(board: Board, trie: Trie, var players: List[Player], bag: Bag) {

  def initializeGame(): Unit = {
    players.foreach(player => player.rack.fillRack(bag))
    board.boardTiles = GameUtilities.initialiseBoard()
  }

  def updateBoard(): Unit = board.updateBoard()

  def gameTurn(isStartingTurn: Boolean, showWordDefinition: Boolean): Unit = {
    if (isStartingTurn) {
      playerMove(players.head, showWordDefinition)
      players.tail.foreach(player => playerMove(player, showWordDefinition))
    } else {
      players.foreach(player => playerMove(player, showWordDefinition))
    }
  }

  private def playerMove(player: Player, showWordDefinition: Boolean): ScoringWord = {
    player.rack.printRack()
    val highestScoringWord = getCorrectAlgorithmMove(player.algorithm, board, trie, player.rack)
    placeHighestScoringWord(player, highestScoringWord, showWordDefinition)
    player.placedWords += highestScoringWord
    player.score += highestScoringWord.score
    player.rack.fillRack(bag)
    updateBoard()
    board.printBoard()
    highestScoringWord
  }

  private def getCorrectAlgorithmMove(algorithm: Algorithm.Value, board: Board,
                                      trie: Trie, rack: Rack): ScoringWord = algorithm match {
    case Algorithm.OPTIMAL =>
      OptimalAlgorithm.findHighestScoringWord(board, trie, rack)
    case Algorithm.GREEDY =>
      GreedyAlgorithm.findHighestScoringWord(board, trie, rack)
  }

  private def placeHighestScoringWord(player: Player, highestScoringWord: ScoringWord,
                                      showWordDefinition: Boolean): Unit = {
    println(s"${player.name} score prior to placing word: ${player.score}")
    println(s"${player.name} placing ${highestScoringWord.direction} word: ${highestScoringWord.word}")
    println(s"Word placed at: ${highestScoringWord.x}, ${highestScoringWord.y} for ${highestScoringWord.score} points.")
    println(s"Used tiles: ${highestScoringWord.tilesUsed.map(tile => tile.letter).mkString(",")}")
    if (showWordDefinition) {
      println(s"Definition: ${WordDefinition.getWordDefinition(highestScoringWord.word)}")
    }
    val newTiles: ListBuffer[PlayerTile] = player.rack.tiles.clone()

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
    player.rack.setRack(newTiles)
  }

  def printFinalScores(): Unit = {
    println(s"Final game scores:")
    println(s"------------------")
    players.foreach(player => {
      println(s"${player.name} ended the game with a final score of: ${player.score}")
      println(s"They played: ${player.placedWords.map(w => w.word).mkString(", ")}")
    })
  }
}
