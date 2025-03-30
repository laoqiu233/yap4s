package org.yap4s.core.parse

import org.yap4s.core.grammar.{Grammar, TerminalTokenSupport}

trait ParserFactory {
  type ProducedParser[+T, -C] <: Parser[C]

  def buildParser[T, C](grammar: Grammar[T, C])(implicit ev: TerminalTokenSupport[C]): ProducedParser[T, C]
}
