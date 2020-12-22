import TileUtilities._

object GameUtilities {
  def initialiseBoard(): Array[Array[BoardTile]] = {
    Array(
      Array(
        TripleWordTile(),
        EmptyBoardTile(), EmptyBoardTile(),
        DoubleLetterTile(),
        EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(),
        TripleWordTile(),
        EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(),
        DoubleLetterTile(),
        EmptyBoardTile(), EmptyBoardTile(),
        TripleWordTile()
      ),
      Array(
        EmptyBoardTile(),
        DoubleWordTile(),
        EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(),
        TripleLetterTile(),
        EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(),
        TripleLetterTile(),
        EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(),
        DoubleWordTile(),
        EmptyBoardTile()
      ),
      Array(
        EmptyBoardTile(), EmptyBoardTile(),
        DoubleWordTile(),
        EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(),
        DoubleLetterTile(),
        EmptyBoardTile(),
        DoubleLetterTile(),
        EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(),
        DoubleWordTile(),
        EmptyBoardTile(), EmptyBoardTile()
      ),
      Array(
        DoubleLetterTile(),
        EmptyBoardTile(), EmptyBoardTile(),
        DoubleWordTile(),
        EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(),
        DoubleLetterTile(),
        EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(),
        DoubleWordTile(),
        EmptyBoardTile(), EmptyBoardTile(),
        DoubleLetterTile()
      ),
      Array(
        EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(),
        DoubleWordTile(),
        EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(),
        DoubleWordTile(),
        EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile()
      ),
      Array(
        EmptyBoardTile(),
        TripleLetterTile(),
        EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(),
        TripleLetterTile(),
        EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(),
        TripleLetterTile(),
        EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(),
        TripleLetterTile(),
        EmptyBoardTile()
      ),
      Array(
        EmptyBoardTile(), EmptyBoardTile(),
        DoubleLetterTile(),
        EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(),
        DoubleLetterTile(),
        EmptyBoardTile(),
        DoubleLetterTile(),
        EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(),
        DoubleLetterTile(),
        EmptyBoardTile(), EmptyBoardTile()
      ),
      Array(
        TripleWordTile(),
        EmptyBoardTile(), EmptyBoardTile(),
        DoubleLetterTile(),
        EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(),
        DoubleWordTile(),
        EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(),
        DoubleLetterTile(),
        EmptyBoardTile(), EmptyBoardTile(),
        TripleWordTile()
      ),
      Array(
        EmptyBoardTile(), EmptyBoardTile(),
        DoubleLetterTile(),
        EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(),
        DoubleLetterTile(),
        EmptyBoardTile(),
        DoubleLetterTile(),
        EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(),
        DoubleLetterTile(),
        EmptyBoardTile(), EmptyBoardTile()
      ),
      Array(
        EmptyBoardTile(),
        TripleLetterTile(),
        EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(),
        TripleLetterTile(),
        EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(),
        TripleLetterTile(),
        EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(),
        TripleLetterTile(),
        EmptyBoardTile()
      ),
      Array(
        EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(),
        DoubleWordTile(),
        EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(),
        DoubleWordTile(),
        EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile()
      ),
      Array(
        DoubleLetterTile(),
        EmptyBoardTile(), EmptyBoardTile(),
        DoubleWordTile(),
        EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(),
        DoubleLetterTile(),
        EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(),
        DoubleWordTile(),
        EmptyBoardTile(), EmptyBoardTile(),
        DoubleLetterTile()
      ),
      Array(
        EmptyBoardTile(), EmptyBoardTile(),
        DoubleWordTile(),
        EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(),
        DoubleLetterTile(),
        EmptyBoardTile(),
        DoubleLetterTile(),
        EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(),
        DoubleWordTile(),
        EmptyBoardTile(), EmptyBoardTile()
      ),
      Array(
        EmptyBoardTile(),
        DoubleWordTile(),
        EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(),
        TripleLetterTile(),
        EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(),
        TripleLetterTile(),
        EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(),
        DoubleWordTile(),
        EmptyBoardTile()
      ),
      Array(
        TripleWordTile(),
        EmptyBoardTile(), EmptyBoardTile(),
        DoubleLetterTile(),
        EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(),
        TripleWordTile(),
        EmptyBoardTile(), EmptyBoardTile(), EmptyBoardTile(),
        DoubleLetterTile(),
        EmptyBoardTile(), EmptyBoardTile(),
        TripleWordTile()
      )
    )
  }
}
