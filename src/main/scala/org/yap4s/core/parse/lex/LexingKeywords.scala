package org.yap4s.core.parse.lex

import org.yap4s.core.grammar.Token.RuleTerminalToken
import org.yap4s.core.grammar.{Grammar, TerminalTokenSupport}
import org.yap4s.core.parse.{ExtractResultParser, ParserFactory}

case class LexingKeywords[C](intersectionTokens: Seq[RuleTerminalToken[C]])
    extends ParserFactory[C] {
  override type ProducedParser[+T, C0] = ExtractResultParser[Seq[T], C0]

  override def buildParser[T, C0 <: C](
      grammar: Grammar[T, C0]
  )(implicit ev: TerminalTokenSupport[C0]): ExtractResultParser[Seq[T], C0] =
    new LexingKeywordsParser(grammar, intersectionTokens)
      with ExtractResultParser[Seq[T], C0]
}
