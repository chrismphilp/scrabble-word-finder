import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;

public class ScrabbleFileReader {
    public Trie readFile(String filename) throws FileNotFoundException {
        File file = new File(filename);
        BufferedReader br = new BufferedReader(new FileReader(file));

        Trie trie = new Trie(null, false, new Trie[26]);

        br.lines().forEach(v -> TrieUtilities.insert(trie, v, 0));
        return trie;
    }
}
