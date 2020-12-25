import TileUtilities._
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class GameTest extends AnyFunSuite {
  val trie: Trie = FileProcessor.convertFileToTrie("collins-scrabble-words-2019.txt")
  val rack: Rack = new Rack(new ListBuffer[PlayerTile])

  test("Should correctly find highest scoring word from single word") {
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

    val game: Game = new Game(board, trie, rack)
    game.updateBoard()

    val highestScoringWord: HighestScoringWord = game.findHighestScoringWord()

    assert(highestScoringWord.word === "APER")
    assert(highestScoringWord.score === 42)
  }

  test("Should correctly update Cross-Check tiles with multiple separated horizontal words") {
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

    val game: Game = new Game(board, trie, rack)
    game.updateBoard()

    val highestScoringWord: HighestScoringWord = game.findHighestScoringWord()

    assert(highestScoringWord.word === "PAR")
    assert(highestScoringWord.score === 27)
  }

  test("Should correctly update Cross-Check tiles with single vertical word") {
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

    val game: Game = new Game(board, trie, rack)
    game.updateBoard()

    val highestScoringWord: HighestScoringWord = game.findHighestScoringWord()

    assert(highestScoringWord.word === "RAPE")
    assert(highestScoringWord.score === 37)
  }

  test("Should correctly update Cross-Check tiles with multiple vertical words") {
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

    val game: Game = new Game(board, trie, rack)
    game.updateBoard()

    val highestScoringWord: HighestScoringWord = game.findHighestScoringWord()

    assert(highestScoringWord.word === "PASH")
    assert(highestScoringWord.score === 18)
  }

  test("Should correctly find highest scoring starting word") {

    val board: Board = new Board(Array(), trie)

    rack.setRack(List(S(), P(), E(), A(), R(), E(), L()).to(ListBuffer))

    val game: Game = new Game(board, trie, rack)
    game.initializeGame()

    val highestScoringWord: HighestScoringWord = game.findHighestScoringWord()

    assert(highestScoringWord.word === "RELAPSE")
    assert(highestScoringWord.score === 74)
  }
}
