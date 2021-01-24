import scala.collection.{MapView, immutable}
import scala.collection.mutable.ListBuffer

object RackHeuristicUtilities {

  private val firstRackHeuristicMapping: immutable.HashMap[Char, Double] = immutable.HashMap(
    'A' -> 1.0, 'B' -> -3.5,
    'C' -> -0.5, 'D' -> 0.0,
    'E' -> 4.0, 'F' -> -2.0,
    'G' -> -2.0, 'H' -> 0.5,
    'I' -> -0.5, 'J' -> -3.0,
    'K' -> -2.5, 'L' -> -1.0,
    'M' -> -1.0, 'N' -> 0.5,
    'O' -> -1.5, 'P' -> -2.5,
    'Q' -> -11.5, 'R' -> 1.5,
    'S' -> 7.5, 'T' -> 0.0,
    'U' -> -3.0, 'V' -> -5.5,
    'W' -> -4.0, 'X' -> 3.5,
    'Y' -> -2.0, 'Z' -> 2.0,
    ' ' -> 24.5)

  private val multipleRackHeuristicMapping: immutable.HashMap[Char, Double] = immutable.HashMap(
    'A' -> -3.0, 'B' -> -3.0,
    'C' -> -3.5, 'D' -> -2.5,
    'E' -> -2.5, 'F' -> -2.0,
    'G' -> -2.5, 'H' -> -3.5,
    'I' -> -4.0, 'L' -> -2.0,
    'M' -> -2.0, 'N' -> -2.5,
    'O' -> -3.5, 'P' -> -2.5,
    'R' -> -3.5, 'S' -> -4.0,
    'T' -> -2.5, 'U' -> -3.0,
    'V' -> -3.5, 'W' -> -4.5,
    'Y' -> -4.5, ' ' -> -15.0)

  def getRackHeuristicValue(letters: ListBuffer[Char]): Double = {
    val letterCounts: MapView[Char, Int] = letters.groupBy(identity).view.mapValues(_.size)
    letterCounts.map(v => getScoreForLetterCount(v)).sum
  }

  private def getScoreForLetterCount(lMap: (Char, Int)): Double = {
    1.to(lMap._2)
      .map(x => firstRackHeuristicMapping(lMap._1) + ((x - 1) * multipleRackHeuristicMapping(lMap._1)))
      .sum
  }

  // x -> vowels, y -> consonants
  private val vowelConsonantRatioScores: List[List[Int]] = List(
    List(0, 0, -1, -2, -3, -4, -5),
    List(-1, 1, 1, 0, -1, -2),
    List(-2, 0, 2, 2, 1),
    List(-3, -1, 1, 3),
    List(-4, -2, 0),
    List(-5, -3),
    List(-6),
  )

  def getVowelConsonantRatioScore(letters: ListBuffer[Char]): Int = {
    val vowelConsonantMappings: MapView[LetterType.Value, Int] = letters
      .map(v => getLetterType(v))
      .groupBy(identity).view.mapValues(_.size)
    getRackLetterRatioScore(vowelConsonantMappings.getOrElse(LetterType.VOWEL, 0),
      vowelConsonantMappings.getOrElse(LetterType.CONSONANT, 0))
  }

  def getLetterType(letter: Char): LetterType.Value = letter match {
    case ' ' => LetterType.BLANK
    case 'A' | 'E' | 'I' | 'O' | 'U' => LetterType.VOWEL
    case _ => LetterType.CONSONANT
  }

  def getRackLetterRatioScore(vowels: Int, consonants: Int): Int = vowelConsonantRatioScores(vowels)(consonants)
}
