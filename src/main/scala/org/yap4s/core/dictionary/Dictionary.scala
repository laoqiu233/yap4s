package org.yap4s.core.dictionary

import org.yap4s.core.grammar.Token.NonTerminalToken
import org.yap4s.core.grammar.{Rule, TerminalTokenSupport}
import org.yap4s.core.trie.TrieNode

trait Dictionary[V, C, I <: Iterable[C]] {
  def getByValue(value: V): Seq[I]
  def getValues: Seq[V]
  def getAll: Seq[(V, I)]

  def toRegularRules(implicit
      ev: TerminalTokenSupport[C]
  ): (NonTerminalToken, Seq[Rule[C]]) = {
    val trie = TrieNode.buildTrie(getAll)
    trie.toRegularRules
  }
}
