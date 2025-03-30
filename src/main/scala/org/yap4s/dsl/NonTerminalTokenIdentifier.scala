package org.yap4s.dsl

import org.yap4s.core.grammar.Token.{NonTerminalToken, SimpleNonTerminalToken}

sealed trait NonTerminalTokenIdentifier[T] {
  def name: String
  val token: NonTerminalToken = SimpleNonTerminalToken(name)
}

object NonTerminalTokenIdentifier {
  case class ExtractableNonTerminalToken[T](name: String) extends NonTerminalTokenIdentifier[T]
  case class FictiveNonTerminalToken(name: String) extends NonTerminalTokenIdentifier[Any]
}