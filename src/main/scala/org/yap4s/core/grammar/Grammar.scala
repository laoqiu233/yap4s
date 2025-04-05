package org.yap4s.core.grammar

import org.yap4s.core.grammar.Token.NonTerminalToken
import org.yap4s.core.grammar.modify.GrammarModification
import org.yap4s.core.parse.ParserFactory

case class Grammar[+T, C: TerminalTokenSupport](
    rules: Seq[Rule[C]],
    startToken: NonTerminalToken,
    appliedModifications: Seq[GrammarModification]
) {
  override def toString: String =
    rules.mkString("\n")

  def parseWith(factory: ParserFactory[C]): factory.ProducedParser[T, C] =
    factory.buildParser(this)
}
