package org.yap4s.core.trie

import org.yap4s.core.grammar.{Rule, TerminalTokenSupport}
import org.yap4s.core.grammar.Rule.CannonRule
import org.yap4s.core.grammar.Token.{NonTerminalToken, SimpleNonTerminalToken}
import org.yap4s.core.transform.{ImmediateExtractor, NonTerminalTokenExtractor}

case class TrieNode[V, C](children: Map[C, TrieNode[V, C]], values: Seq[V]) {
  def toRegularRules(implicit
      ev: TerminalTokenSupport[C]
  ): (NonTerminalToken, Seq[Rule[C]]) = {
    val label = s"trie_${hashCode()}_"
    toRegularRulesInner(label)
  }

  private def toRegularRulesInner(
      label: String
  )(implicit ev: TerminalTokenSupport[C]): (NonTerminalToken, Seq[Rule[C]]) = {
    val currToken = SimpleNonTerminalToken(label)

    val rules = children.flatMap { case (char, child) =>
      val (childToken, childRules) = child.toRegularRulesInner(label + char)
      val parentRule = CannonRule(
        currToken,
        Seq(ev.wrap(char), childToken),
        new NonTerminalTokenExtractor[Seq[V]](1, childToken)
      )
      childRules :+ parentRule
    }.toSeq

    val rulesWithValue =
      if (values.isEmpty)
        rules
      else {
        val valueRule =
          CannonRule[C](currToken, Nil, new ImmediateExtractor(values))
        rules :+ valueRule
      }

    (currToken, rulesWithValue)
  }
}

object TrieNode {
  def buildTrie[V, C](values: Seq[(V, Iterable[C])]): TrieNode[V, C] = {
    values.foldLeft(TrieNode[V, C](Map.empty, Nil)) {
      case (node, (value, chars)) =>
        addKVToTrie(node, chars.toList, value)
    }
  }

  private def addKVToTrie[V, C](
      node: TrieNode[V, C],
      key: List[C],
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
