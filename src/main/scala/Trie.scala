class Trie(val value: Char,
           var isComplete: Boolean = false,
           var completedWord: String = "",
           var children: Array[Trie] = new Array[Trie](26))
