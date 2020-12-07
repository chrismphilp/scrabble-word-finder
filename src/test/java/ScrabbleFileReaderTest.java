import org.junit.Before;
import org.junit.Test;

import java.io.FileNotFoundException;

import static org.junit.Assert.assertTrue;

public class ScrabbleFileReaderTest {

    private ScrabbleFileReader scrabbleFileReader;


    @Before
    public void setUp() {
        this.scrabbleFileReader = new ScrabbleFileReader();
    }

    @Test
    public void shouldCorrectlyReadInScrabbleDictionary() throws FileNotFoundException {
        Trie trie = scrabbleFileReader.readFile("collins_scrabble_words_2019.txt");
        assertTrue(TrieUtilities.search(trie, "TRAMP", 0));
    }
}