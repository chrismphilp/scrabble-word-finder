import GameUtilities.initialiseBoard
import TileUtilities._
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class GameTest extends AnyFunSuite {
  val trie: Trie = FileProcessor.convertFileToTrie("collins-scrabble-words-2019.txt")
  val rack: Rack = new Rack(new ListBuffer[PlayerTile])
  val bag: Bag = createInitialBag()

  test("Should correctly find highest scoring word from single horizontal word") {
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

    val game: Game = new Game(board, trie, rack, bag)
    game.updateBoard()

    val highestScoringWord: HighestScoringWord = game.findHighestScoringWord(false)

    assert(highestScoringWord.word === "APER")
    assert(highestScoringWord.score === 42)
  }

  test("Should correctly find highest scoring word from multiple separated horizontal words") {
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

    val game: Game = new Game(board, trie, rack, bag)
    game.updateBoard()

    val highestScoringWord: HighestScoringWord = game.findHighestScoringWord(false)

    assert(highestScoringWord.word === "PAR")
    assert(highestScoringWord.score === 27)
  }

  test("Should correctly find highest scoring word from single vertical word") {
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

    val game: Game = new Game(board, trie, rack, bag)
    game.updateBoard()

    val highestScoringWord: HighestScoringWord = game.findHighestScoringWord(false)

    assert(highestScoringWord.word === "RAPE")
    assert(highestScoringWord.score === 37)
  }

  test("Should correctly find highest scoring word from multiple separated vertical words") {
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

    val game: Game = new Game(board, trie, rack, bag)
    game.updateBoard()

    val highestScoringWord: HighestScoringWord = game.findHighestScoringWord(false)

    assert(highestScoringWord.word === "PASH")
    assert(highestScoringWord.score === 18)
  }

  test("Should correctly find highest scoring starting word") {

    val board: Board = new Board(Array(), trie)

    rack.setRack(List(S(), P(), E(), A(), R(), E(), L()).to(ListBuffer))
    val game: Game = new Game(board, trie, rack, bag)
    game.initializeGame()
    var highestScoringWord: HighestScoringWord = game.findHighestScoringWord(true)

    assert(highestScoringWord.word === "RELAPSE")
    assert(highestScoringWord.score === 74)

    rack.setRack(List(D(), E(), Y(), N(), O(), R(), T()).to(ListBuffer))
    game.initializeGame()
    highestScoringWord = game.findHighestScoringWord(true)

    assert(highestScoringWord.word === "RODNEY")
    assert(highestScoringWord.score === 28)

    rack.setRack(List(Z(), E(), L(), N(), O(), R(), S()).to(ListBuffer))
    game.initializeGame()
    highestScoringWord = game.findHighestScoringWord(true)

    assert(highestScoringWord.word === "ZONERS")
    assert(highestScoringWord.score === 50)
  }

  test("Should correctly simulate highest scoring word for each tile placed") {

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
    var game: Game = new Game(board, trie, rack, bag)
    game.updateBoard()

    var highestScoringWord: HighestScoringWord = game.findHighestScoringWord(false)

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

    game = new Game(board, trie, rack, bag)
    game.updateBoard()
    highestScoringWord = game.findHighestScoringWord(false)

    assert(highestScoringWord.word === "PLEASERS")
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
    game = new Game(board, trie, rack, bag)
    game.updateBoard()
    highestScoringWord = game.findHighestScoringWord(false)

    assert(highestScoringWord.word === "QUOTED")
    assert(highestScoringWord.score === 39)

    board.boardTiles(1)(11) = FilledBoardTile(Q())
    board.boardTiles(2)(11) = FilledBoardTile(U())
    board.boardTiles(3)(11) = FilledBoardTile(O())
    board.boardTiles(4)(11) = FilledBoardTile(T())
    board.boardTiles(5)(11) = FilledBoardTile(E())
    board.boardTiles(6)(11) = FilledBoardTile(D())

    rack.setRack(List(U(), V(), D(), I(), A(), O(), X()).to(ListBuffer))
    game = new Game(board, trie, rack, bag)
    game.updateBoard()
    highestScoringWord = game.findHighestScoringWord(false)

    assert(highestScoringWord.word === "DOUX")
    assert(highestScoringWord.score === 47)

    board.boardTiles(0)(6) = FilledBoardTile(D())
    board.boardTiles(0)(7) = FilledBoardTile(O())
    board.boardTiles(0)(8) = FilledBoardTile(U())
    board.boardTiles(0)(9) = FilledBoardTile(X())

    rack.setRack(List(C(), V(), Y(), I(), A(), B(), E()).to(ListBuffer))
    game = new Game(board, trie, rack, bag)
    game.updateBoard()
    highestScoringWord = game.findHighestScoringWord(false)

    game.printBoard()

    assert(board.boardTiles(6)(6).verticalCrossChecks.contains('Y') === true)

    assert(highestScoringWord.word === "BEVY")
    assert(highestScoringWord.score === 30)
  }

  test("Should correctly simulate whole game without errors") {

    val board: Board = new Board(initialiseBoard(), trie)
    val game: Game = new Game(board, trie, rack, bag)
    game.updateRack()
    rack.printRack()
    game.placeHighestScoringWord(game.findHighestScoringWord(true))
    game.updateRack()
    game.printBoard()

    while (rack.tiles.nonEmpty) {
      game.updateBoard()
      val highestScoringWord = game.findHighestScoringWord(false)
      if (highestScoringWord.word === "") rack.tiles = ListBuffer.empty
      rack.printRack()
      game.placeHighestScoringWord(highestScoringWord)
      game.updateRack()
      game.printBoard()
    }
    println("Done")
  }
}
