import GameUtilities.initialiseBoard
import TileUtilities._
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class GameTest extends AnyFunSuite {
  val trie: Trie = FileProcessor.convertFileToTrie("collins-scrabble-words-2019.txt")

  test("Should correctly find highest scoring word from single horizontal word") {
    val rack: Rack = new Rack(new ListBuffer[PlayerTile])
    val player1: Player = new Player("Player1", rack, new ListBuffer[ScoringWord])
    val players: List[Player] = List(player1)
    val bag: Bag = createInitialBag()

    val board: Board = new Board(Array(
      Array(EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile()),
      Array(EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile()),
      Array(
        TripleWordTile(),
        new BoardTile(Option(A()), Multiplier.NONE, new mutable.HashMap[Char, Int], false,
          false, new mutable.HashMap[Char, Int], false,
          false, false),
        new BoardTile(Option(P()), Multiplier.NONE, new mutable.HashMap[Char, Int], false,
          false, new mutable.HashMap[Char, Int], false,
          false, false),
        new BoardTile(Option(E()), Multiplier.NONE, new mutable.HashMap[Char, Int], false,
          false, new mutable.HashMap[Char, Int], false,
          false, false),
        EmptyBoardTile()
      ),
      Array(EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile()),
      Array(EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile())
    ), trie)

    rack.setRack(List(A(), P(), E(), R()).to(ListBuffer))

    val game: Game = new Game(board, trie, players, bag)
    game.updateBoard()

    val highestScoringWord: ScoringWord = GreedyAlgorithm
      .findHighestScoringWord(board, trie, rack, isStartingWord = false)

    assert(highestScoringWord.score === 42)
  }

  test("Should correctly find highest scoring word from multiple separated horizontal words") {
    val rack: Rack = new Rack(new ListBuffer[PlayerTile])
    val player1: Player = new Player("Player1", rack, new ListBuffer[ScoringWord])
    val players: List[Player] = List(player1)
    val bag: Bag = createInitialBag()
    val board: Board = new Board(Array(
      Array(
        new BoardTile(Option(G()), Multiplier.NONE, new mutable.HashMap[Char, Int], false,
          false, new mutable.HashMap[Char, Int], false,
          false, false),
        new BoardTile(Option(R()), Multiplier.NONE, new mutable.HashMap[Char, Int], false,
          false, new mutable.HashMap[Char, Int], false,
          false, false),
        new BoardTile(Option(A()), Multiplier.NONE, new mutable.HashMap[Char, Int], false,
          false, new mutable.HashMap[Char, Int], false,
          false, false),
        new BoardTile(Option(D()), Multiplier.NONE, new mutable.HashMap[Char, Int], false,
          false, new mutable.HashMap[Char, Int], false,
          false, false),
        new BoardTile(Option(E()), Multiplier.NONE, new mutable.HashMap[Char, Int], false,
          false, new mutable.HashMap[Char, Int], false,
          false, false)
      ),
      Array(EmptyBoardTile(), TripleLetterTile(), EmptyBoardTile(), DoubleWordTile(), EmptyBoardTile()),
      Array(
        EmptyBoardTile(),
        new BoardTile(Option(A()), Multiplier.NONE, new mutable.HashMap[Char, Int], false,
          false, new mutable.HashMap[Char, Int], false,
          false, false),
        new BoardTile(Option(P()), Multiplier.NONE, new mutable.HashMap[Char, Int], false,
          false, new mutable.HashMap[Char, Int], false,
          false, false),
        new BoardTile(Option(E()), Multiplier.NONE, new mutable.HashMap[Char, Int], false,
          false, new mutable.HashMap[Char, Int], false,
          false, false),
        EmptyBoardTile()
      ),
      Array(EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile()),
      Array(EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile())
    ), trie)

    rack.setRack(List(A(), P(), E(), R()).to(ListBuffer))

    val game: Game = new Game(board, trie, players, bag)
    game.updateBoard()

    val highestScoringWord: ScoringWord = GreedyAlgorithm
      .findHighestScoringWord(board, trie, rack, isStartingWord = false)

    assert(highestScoringWord.score === 27)
  }

  test("Should correctly find highest scoring word from single vertical word") {
    val rack: Rack = new Rack(new ListBuffer[PlayerTile])
    val player1: Player = new Player("Player1", rack, new ListBuffer[ScoringWord])
    val players: List[Player] = List(player1)
    val bag: Bag = createInitialBag()
    val board: Board = new Board(Array(
      Array(EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile()),
      Array(
        EmptyBoardTile(),
        EmptyBoardTile(),
        EmptyBoardTile(),
        EmptyBoardTile(),
        new BoardTile(Option(H()), Multiplier.NONE, new mutable.HashMap[Char, Int], false,
          false, new mutable.HashMap[Char, Int], false,
          false, false)
      ),
      Array(
        EmptyBoardTile(),
        EmptyBoardTile(),
        EmptyBoardTile(),
        TripleWordTile(),
        new BoardTile(Option(E()), Multiplier.NONE, new mutable.HashMap[Char, Int], false,
          false, new mutable.HashMap[Char, Int], false,
          false, false)
      ),
      Array(
        EmptyBoardTile(),
        EmptyBoardTile(),
        EmptyBoardTile(),
        EmptyBoardTile(),
        new BoardTile(Option(A()), Multiplier.NONE, new mutable.HashMap[Char, Int], false,
          false, new mutable.HashMap[Char, Int], false,
          false, false)
      ),
      Array(
        EmptyBoardTile(),
        EmptyBoardTile(),
        EmptyBoardTile(),
        EmptyBoardTile(),
        new BoardTile(Option(P()), Multiplier.NONE, new mutable.HashMap[Char, Int], false,
          false, new mutable.HashMap[Char, Int], false,
          false, false)
      )
    ), trie)

    rack.setRack(List(A(), P(), E(), R()).to(ListBuffer))

    val game: Game = new Game(board, trie, players, bag)
    game.updateBoard()

    val highestScoringWord: ScoringWord = GreedyAlgorithm
      .findHighestScoringWord(board, trie, rack, isStartingWord = false)

    assert(highestScoringWord.word === "RAPE")
    assert(highestScoringWord.score === 37)
  }

  test("Should correctly find highest scoring word from multiple separated vertical words") {
    val rack: Rack = new Rack(new ListBuffer[PlayerTile])
    val player1: Player = new Player("Player1", rack, new ListBuffer[ScoringWord])
    val players: List[Player] = List(player1)
    val bag: Bag = createInitialBag()
    val board: Board = new Board(Array(
      Array(
        EmptyBoardTile(),
        EmptyBoardTile(),
        new BoardTile(Option(C()), Multiplier.NONE, new mutable.HashMap[Char, Int], false,
          false, new mutable.HashMap[Char, Int], false,
          false, false),
        EmptyBoardTile(),
        EmptyBoardTile()
      ),
      Array(
        EmptyBoardTile(),
        EmptyBoardTile(),
        new BoardTile(Option(A()), Multiplier.NONE, new mutable.HashMap[Char, Int], false,
          false, new mutable.HashMap[Char, Int], false,
          false, false),
        DoubleWordTile(),
        new BoardTile(Option(H()), Multiplier.NONE, new mutable.HashMap[Char, Int], false,
          false, new mutable.HashMap[Char, Int], false,
          false, false)
      ),
      Array(
        EmptyBoardTile(),
        EmptyBoardTile(),
        new BoardTile(Option(S()), Multiplier.NONE, new mutable.HashMap[Char, Int], false,
          false, new mutable.HashMap[Char, Int], false,
          false, false),
        EmptyBoardTile(),
        new BoardTile(Option(E()), Multiplier.NONE, new mutable.HashMap[Char, Int], false,
          false, new mutable.HashMap[Char, Int], false,
          false, false)
      ),
      Array(
        EmptyBoardTile(),
        EmptyBoardTile(),
        new BoardTile(Option(H()), Multiplier.NONE, new mutable.HashMap[Char, Int], false,
          false, new mutable.HashMap[Char, Int], false,
          false, false),
        EmptyBoardTile(),
        new BoardTile(Option(A()), Multiplier.NONE, new mutable.HashMap[Char, Int], false,
          false, new mutable.HashMap[Char, Int], false,
          false, false)
      ),
      Array(
        EmptyBoardTile(),
        EmptyBoardTile(),
        EmptyBoardTile(),
        EmptyBoardTile(),
        new BoardTile(Option(P()), Multiplier.NONE, new mutable.HashMap[Char, Int], false,
          false, new mutable.HashMap[Char, Int], false,
          false, false)
      )
    ), trie)

    rack.setRack(List(S(), P(), E(), R()).to(ListBuffer))

    val game: Game = new Game(board, trie, players, bag)
    game.updateBoard()

    val highestScoringWord: ScoringWord = GreedyAlgorithm
      .findHighestScoringWord(board, trie, rack, isStartingWord = false)

    assert(highestScoringWord.word === "PASH")
    assert(highestScoringWord.score === 18)
  }

  test("Should correctly find highest scoring starting word") {
    val rack: Rack = new Rack(new ListBuffer[PlayerTile])
    val player1: Player = new Player("Player1", rack, new ListBuffer[ScoringWord])
    val players: List[Player] = List(player1)
    val bag: Bag = createInitialBag()
    val board: Board = new Board(Array(), trie)

    rack.setRack(List(S(), P(), E(), A(), R(), E(), L()).to(ListBuffer))
    val game: Game = new Game(board, trie, players, bag)
    game.initializeGame()
    var highestScoringWord: ScoringWord = GreedyAlgorithm
      .findHighestScoringWord(board, trie, rack, isStartingWord = true)

    assert(highestScoringWord.score === 74)

    rack.setRack(List(D(), E(), Y(), N(), O(), R(), T()).to(ListBuffer))
    game.initializeGame()
    highestScoringWord = GreedyAlgorithm
      .findHighestScoringWord(board, trie, rack, isStartingWord = true)

    assert(highestScoringWord.score === 28)

    rack.setRack(List(Z(), E(), L(), N(), O(), R(), S()).to(ListBuffer))
    game.initializeGame()
    highestScoringWord = GreedyAlgorithm
      .findHighestScoringWord(board, trie, rack, isStartingWord = true)

    assert(highestScoringWord.score === 50)
  }

  test("Should correctly simulate highest scoring word for each tile placed") {
    val rack: Rack = new Rack(new ListBuffer[PlayerTile])
    val player1: Player = new Player("Player1", rack, new ListBuffer[ScoringWord])
    val players: List[Player] = List(player1)
    val bag: Bag = createInitialBag()
    val board: Board = new Board(initialiseBoard(), trie)

    board.boardTiles(4)(4) = FilledBoardTile(M())
    board.boardTiles(4)(5) = FilledBoardTile(I())
    board.boardTiles(4)(6) = FilledBoardTile(R())
    board.boardTiles(4)(7) = FilledBoardTile(T())
    board.boardTiles(4)(8) = FilledBoardTile(H())

    board.boardTiles(4)(8) = FilledBoardTile(H())
    board.boardTiles(5)(8) = FilledBoardTile(E())
    board.boardTiles(6)(8) = FilledBoardTile(A())
    board.boardTiles(7)(8) = FilledBoardTile(T())
    board.boardTiles(8)(8) = FilledBoardTile(E())
    board.boardTiles(9)(8) = FilledBoardTile(D())

    board.boardTiles(7)(5) = FilledBoardTile(D())
    board.boardTiles(7)(6) = FilledBoardTile(E())
    board.boardTiles(7)(7) = FilledBoardTile(A())
    board.boardTiles(7)(8) = FilledBoardTile(T())
    board.boardTiles(7)(9) = FilledBoardTile(H())

    rack.setRack(List(S(), P(), E(), A(), R(), E(), L()).to(ListBuffer))
    var game: Game = new Game(board, trie, players, bag)
    game.updateBoard()

    var highestScoringWord: ScoringWord = GreedyAlgorithm
      .findHighestScoringWord(board, trie, rack, isStartingWord = false)

    assert(highestScoringWord.word === "PRESALE")
    assert(highestScoringWord.score === 96)

    board.boardTiles(1)(5) = FilledBoardTile(S())
    board.boardTiles(2)(5) = FilledBoardTile(P())
    board.boardTiles(3)(5) = FilledBoardTile(O())
    board.boardTiles(4)(5) = FilledBoardTile(I())
    board.boardTiles(5)(5) = FilledBoardTile(L())
    board.boardTiles(6)(5) = FilledBoardTile(E())
    board.boardTiles(7)(5) = FilledBoardTile(D())

    board.boardTiles(5)(10) = FilledBoardTile(M())
    board.boardTiles(6)(10) = FilledBoardTile(I())
    board.boardTiles(7)(10) = FilledBoardTile(S())
    board.boardTiles(8)(10) = FilledBoardTile(S())

    game = new Game(board, trie, players, bag)
    game.updateBoard()
    highestScoringWord = GreedyAlgorithm
      .findHighestScoringWord(board, trie, rack, isStartingWord = false)

    assert(highestScoringWord.score === 70)

    board.boardTiles(1)(1) = FilledBoardTile(P())
    board.boardTiles(1)(2) = FilledBoardTile(L())
    board.boardTiles(1)(3) = FilledBoardTile(E())
    board.boardTiles(1)(4) = FilledBoardTile(A())
    board.boardTiles(1)(5) = FilledBoardTile(S())
    board.boardTiles(1)(6) = FilledBoardTile(E())
    board.boardTiles(1)(7) = FilledBoardTile(R())
    board.boardTiles(1)(8) = FilledBoardTile(S())

    rack.setRack(List(Q(), U(), O(), T(), E(), D(), U()).to(ListBuffer))
    game = new Game(board, trie, players, bag)
    game.updateBoard()
    highestScoringWord = GreedyAlgorithm
      .findHighestScoringWord(board, trie, rack, isStartingWord = false)

    assert(highestScoringWord.score === 39)

    board.boardTiles(1)(11) = FilledBoardTile(Q())
    board.boardTiles(2)(11) = FilledBoardTile(U())
    board.boardTiles(3)(11) = FilledBoardTile(O())
    board.boardTiles(4)(11) = FilledBoardTile(T())
    board.boardTiles(5)(11) = FilledBoardTile(E())
    board.boardTiles(6)(11) = FilledBoardTile(D())

    rack.setRack(List(U(), V(), D(), I(), A(), O(), X()).to(ListBuffer))
    game = new Game(board, trie, players, bag)
    game.updateBoard()
    highestScoringWord = GreedyAlgorithm
      .findHighestScoringWord(board, trie, rack, isStartingWord = false)

    assert(highestScoringWord.word === "DOUX")
    assert(highestScoringWord.score === 47)

    board.boardTiles(0)(6) = FilledBoardTile(D())
    board.boardTiles(0)(7) = FilledBoardTile(O())
    board.boardTiles(0)(8) = FilledBoardTile(U())
    board.boardTiles(0)(9) = FilledBoardTile(X())

    rack.setRack(List(C(), V(), Y(), I(), A(), B(), E()).to(ListBuffer))
    game = new Game(board, trie, players, bag)
    game.updateBoard()
    highestScoringWord = GreedyAlgorithm
      .findHighestScoringWord(board, trie, rack, isStartingWord = false)

    assert(board.boardTiles(6)(6).verticalCrossChecks.contains('Y') === true)

    assert(highestScoringWord.score === 30)
  }

  test("Should correctly simulate whole single player game without errors") {

    val board: Board = new Board(initialiseBoard(), trie)
    val bag: Bag = createInitialBag()
    val player1: Player = new Player("Player1", new Rack(new ListBuffer[PlayerTile]), new ListBuffer[ScoringWord])
    val players: List[Player] = List(player1)
    val game: Game = new Game(board, trie, players, bag)
    var gameInProgress: Boolean = true
    var tilesPlaced: Int = 100 - bag.tiles.size

    assertResult(0)(tilesPlaced)
    players.foreach(player => assertResult(player.rack.tiles.length)(0))
    board.printBoard()

    game.initializeGame()

    game.gameTurn(isStartingTurn = true)
    tilesPlaced += players.map(player => player.placedWords.last.tilesUsed.length).sum

    while (gameInProgress) {
      game.gameTurn(isStartingTurn = false)
      tilesPlaced += players.map(player => player.placedWords.last.tilesUsed.length).sum
      if (players.map(player => player.placedWords.last.word).contains("")) gameInProgress = false
    }
//    assertResult(100)(tilesPlaced)
//    assertResult(100)(board.countFilledBoardTiles())
    game.printFinalScores()
  }

  test("Should correctly simulate two player player game without errors") {

    val board: Board = new Board(initialiseBoard(), trie)
    val bag: Bag = createInitialBag()
    val player1: Player = new Player("Player1", new Rack(new ListBuffer[PlayerTile]), new ListBuffer[ScoringWord])
    val player2: Player = new Player("Player2", new Rack(new ListBuffer[PlayerTile]), new ListBuffer[ScoringWord])
    val players: List[Player] = List(player1, player2)
    val game: Game = new Game(board, trie, players, bag)
    var gameInProgress: Boolean = true
    var tilesPlaced: Int = 100 - bag.tiles.size

    assertResult(0)(tilesPlaced)
    players.foreach(player => assertResult(player.rack.tiles.length)(0))
    board.printBoard()

    game.initializeGame()

    game.gameTurn(isStartingTurn = true)
    tilesPlaced += players.map(player => player.placedWords.last.tilesUsed.length).sum

    while (gameInProgress) {
      game.gameTurn(isStartingTurn = false)
      tilesPlaced += players.map(player => player.placedWords.last.tilesUsed.length).sum
      if (players.map(player => player.placedWords.last.word).contains("")) gameInProgress = false
    }
//    assertResult(100)(tilesPlaced)
//    assertResult(100)(board.countFilledBoardTiles())
    game.printFinalScores()
  }
}
