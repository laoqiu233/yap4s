package org.yap4s.core.trie

case class TrieNode[V, C](children: Map[C, TrieNode[V, C]], values: Seq[V])

object TrieNode {
  def buildTrie[V, C](values: Seq[(V, Iterable[C])]): TrieNode[V, C] = {
    values.foldLeft(TrieNode[V, C](Map.empty, Nil)) {
      case (node, (value, chars)) =>
        chars.foldLeft(node) { case (node, char) =>
          node.children.get(char) match {
            case Some(child) =>
              node.copy(children = node.children + (char -> child))
            case None =>
              node.copy(children =
                node.children + (char -> TrieNode(Map.empty, Seq(value)))
              )
          }
        }
    }
  }

  private def addKVToTrie[V, C](
      node: TrieNode[V, C],
      key: Iterable[C],
      value: V
  ): TrieNode[V, C] =
    key match {
      case Nil =>
        node.copy(values = node.values :+ value)
      case head :: tail =>
        val nextNode = node.children.getOrElse(
          head,
          TrieNode(Map.empty[C, TrieNode[V, C]], Nil)
        )
        node.copy(children =
          node.children + (head -> addKVToTrie(nextNode, tail, value))
        )
    }
}
