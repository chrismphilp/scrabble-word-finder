import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable.ListBuffer

class RackHeuristicUtilitiesTest extends AnyFunSuite {
  test("Should return correct rack heuristic for letters") {
    assertResult(-2.5)(RackHeuristicUtilities.getRackHeuristicValue(ListBuffer('I', 'I', 'I', 'S', 'S')))
    assertResult(-42.5)(RackHeuristicUtilities.getRackHeuristicValue(ListBuffer('I', 'I', 'I', 'I', 'I')))
    assertResult(-2.5)(RackHeuristicUtilities.getRackHeuristicValue(ListBuffer('S', 'S', 'S', 'S', 'S')))
  }

  test("Should return correct rack vowel/consonant ratio score") {
    assertResult(1)(RackHeuristicUtilities.getVowelConsonantRatioScore(ListBuffer('I', 'I', 'I', 'S', 'S')))
    assertResult(-5)(RackHeuristicUtilities.getVowelConsonantRatioScore(ListBuffer('I', 'I', 'I', 'I', 'I')))
    assertResult(-4)(RackHeuristicUtilities.getVowelConsonantRatioScore(ListBuffer('S', 'S', 'S', 'S', 'S')))
  }
}
