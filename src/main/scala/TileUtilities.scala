import scala.collection.{immutable, mutable}
import scala.collection.mutable.ListBuffer

object TileUtilities {
  var tileScoreMappings: immutable.HashMap[Char, Int] = immutable.HashMap(
    'A' -> 1,
    'B' -> 3,
    'C' -> 3,
    'D' -> 2,
    'E' -> 1,
    'F' -> 4,
    'G' -> 2,
    'H' -> 4,
    'I' -> 1,
    'J' -> 8,
    'K' -> 5,
    'L' -> 1,
    'M' -> 3,
    'N' -> 1,
    'O' -> 1,
    'P' -> 3,
    'Q' -> 10,
    'R' -> 1,
    'S' -> 1,
    'T' -> 1,
    'U' -> 1,
    'V' -> 4,
    'W' -> 4,
    'X' -> 8,
    'Y' -> 4,
    'Z' -> 10
  )

  def createInitialBag(): Bag = {
    val buffer: ListBuffer[PlayerTile] = new ListBuffer[PlayerTile]
    for (_ <- 0 until 9) buffer += A()
    for (_ <- 0 until 2) buffer += B()
    for (_ <- 0 until 2) buffer += C()
    for (_ <- 0 until 4) buffer += D()
    for (_ <- 0 until 12) buffer += E()
    for (_ <- 0 until 2) buffer += F()
    for (_ <- 0 until 3) buffer += G()
    for (_ <- 0 until 2) buffer += H()
    for (_ <- 0 until 9) buffer += I()
    for (_ <- 0 until 1) buffer += J()
    for (_ <- 0 until 1) buffer += K()
    for (_ <- 0 until 4) buffer += L()
    for (_ <- 0 until 2) buffer += M()
    for (_ <- 0 until 6) buffer += N()
    for (_ <- 0 until 8) buffer += O()
    for (_ <- 0 until 2) buffer += P()
    for (_ <- 0 until 1) buffer += Q()
    for (_ <- 0 until 6) buffer += R()
    for (_ <- 0 until 4) buffer += S()
    for (_ <- 0 until 6) buffer += T()
    for (_ <- 0 until 4) buffer += U()
    for (_ <- 0 until 2) buffer += V()
    for (_ <- 0 until 2) buffer += W()
    for (_ <- 0 until 1) buffer += X()
    for (_ <- 0 until 2) buffer += Y()
    for (_ <- 0 until 1) buffer += Z()
    for (_ <- 0 until 2) buffer += Blank()
    new Bag(buffer)
  }

  def A(): PlayerTile = new PlayerTile('A', 1)
  def B(): PlayerTile = new PlayerTile('B', 3)
  def C(): PlayerTile = new PlayerTile('C', 3)
  def D(): PlayerTile = new PlayerTile('D', 2)
  def E(): PlayerTile = new PlayerTile('E', 1)
  def F(): PlayerTile = new PlayerTile('F', 4)
  def G(): PlayerTile = new PlayerTile('G', 2)
  def H(): PlayerTile = new PlayerTile('H', 4)
  def I(): PlayerTile = new PlayerTile('I', 1)
  def J(): PlayerTile = new PlayerTile('J', 8)
  def K(): PlayerTile = new PlayerTile('K', 5)
  def L(): PlayerTile = new PlayerTile('L', 1)
  def M(): PlayerTile = new PlayerTile('M', 3)
  def N(): PlayerTile = new PlayerTile('N', 1)
  def O(): PlayerTile = new PlayerTile('O', 1)
  def P(): PlayerTile = new PlayerTile('P', 3)
  def Q(): PlayerTile = new PlayerTile('Q', 10)
  def R(): PlayerTile = new PlayerTile('R', 1)
  def S(): PlayerTile = new PlayerTile('S', 1)
  def T(): PlayerTile = new PlayerTile('T', 1)
  def U(): PlayerTile = new PlayerTile('U', 1)
  def V(): PlayerTile = new PlayerTile('V', 4)
  def W(): PlayerTile = new PlayerTile('W', 4)
  def X(): PlayerTile = new PlayerTile('X', 8)
  def Y(): PlayerTile = new PlayerTile('Y', 4)
  def Z(): PlayerTile = new PlayerTile('Z', 10)
  def Blank(): PlayerTile = new PlayerTile(' ', 0)

  def getTileScore(c: Char): Int = tileScoreMappings.getOrElse(c, 0)

  def getTileMultiplierValue(multiplier: Multiplier.Value): Int = multiplier match {
    case Multiplier.DOUBLE_LETTER => 2
    case Multiplier.TRIPLE_LETTER => 3
    case _ => 1
  }

  def getWordMultiplierValue(multiplier: Multiplier.Value): Int = multiplier match {
    case Multiplier.DOUBLE_WORD => 2
    case Multiplier.TRIPLE_WORD => 3
    case _ => 1
  }

  def EmptyBoardTile(): BoardTile = new BoardTile(
    None, Multiplier.NONE, new mutable.HashMap[Char, Int], false, false,
    new mutable.HashMap[Char, Int], false,
    false, false
  )

  def DoubleLetterTile(): BoardTile = new BoardTile(
    None, Multiplier.DOUBLE_LETTER, new mutable.HashMap[Char, Int], false, false,
    new mutable.HashMap[Char, Int], false,
    false, false
  )

  def TripleLetterTile(): BoardTile = new BoardTile(
    None, Multiplier.TRIPLE_LETTER, new mutable.HashMap[Char, Int], false, false,
    new mutable.HashMap[Char, Int], false,
    false, false
  )

  def DoubleWordTile(): BoardTile = new BoardTile(
    None, Multiplier.DOUBLE_WORD, new mutable.HashMap[Char, Int], false, false,
    new mutable.HashMap[Char, Int], false,
    false, false
  )

  def TripleWordTile(): BoardTile = new BoardTile(
    None, Multiplier.TRIPLE_WORD, new mutable.HashMap[Char, Int], false, false,
    new mutable.HashMap[Char, Int], false,
    false, false
  )
}
