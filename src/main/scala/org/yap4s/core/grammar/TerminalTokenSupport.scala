package org.yap4s.core.grammar

import org.yap4s.core.grammar.Token.RuleTerminalToken
import org.yap4s.dsl.definitions.RightHandSideDefinition.PlainDefinition0

trait TerminalTokenSupport[T] extends (T => PlainDefinition0[T]) {
  def wrap(t: T): RuleTerminalToken[T]
  override def apply(v1: T): PlainDefinition0[T] = PlainDefinition0(
    Seq(wrap(v1))
  )
}
