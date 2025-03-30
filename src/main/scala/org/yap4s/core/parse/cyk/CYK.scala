package org.yap4s.core.parse.cyk

import org.yap4s.core.grammar.modify.GrammarModification
import org.yap4s.core.grammar.{Grammar, TerminalTokenSupport}
import org.yap4s.core.parse.{ExtractResultParser, ParserFactory, ReverseModificationsParser}

object CYK extends ParserFactory {
  override type ProducedParser[+T, -C] = ExtractResultParser[T, C]

  override def buildParser[T, C](grammar: Grammar[T, C])(implicit ev: TerminalTokenSupport[C]): ExtractResultParser[T, C] =
    new CYKParser[C](grammar)(ev) with ExtractResultParser[T, C] with ReverseModificationsParser[C] {
      override protected def appliedModifications: Seq[GrammarModification] = grammar.appliedModifications
    }
}
