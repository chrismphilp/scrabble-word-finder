import lombok.experimental.UtilityClass;

import static java.util.Objects.nonNull;

@UtilityClass
public class TrieUtilities {

    public boolean search(Trie curr, String word, int step) {
        if (word.length() == step) return curr.isComplete();
        else {
            char c = word.charAt(step);
            if (curr.getChildren()[c - 65] == null) return false;
            else return search(curr.getChildren()[c - 65], word, step + 1);
        }
    }

    public void insert(Trie curr, String word, int step) {
        if (word.length() == step) curr.setComplete(true);
        else {
            char c = word.charAt(step);
            if (!nonNull(curr.getChildren()[c - 65])) {
                curr.getChildren()[c - 65] = new Trie(c, false, new Trie[26]);
            }
            insert(curr.getChildren()[c - 65], word, step + 1);
        }
    }
}
