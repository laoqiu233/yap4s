package org.yap4s.core.grammar.modify

import org.yap4s.core.grammar.{Grammar, TerminalTokenSupport}

trait GrammarModifier {
  def modificationLabel: GrammarModification
  def modifyGrammar[T, C: TerminalTokenSupport](grammar: Grammar[T, C]): Grammar[T, C]
}
