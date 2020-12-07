trait Trie[T] {
  def values: Trie[T]
  def isWord: Boolean
}
