package org.yap4s.core.parse

import org.yap4s.core.grammar.{Grammar, TerminalTokenSupport}

trait ParserFactory[-C] {
  type ProducedParser[+T, C0 <: C] <: Parser[C0]

  def buildParser[T, C0 <: C](grammar: Grammar[T, C0])(implicit
      ev: TerminalTokenSupport[C0]
  ): ProducedParser[T, C0]
}
