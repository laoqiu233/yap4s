package org.yap4s.core.grammar

sealed trait Token {
  def isTerminal: Boolean
  def isNonTerminal: Boolean = !isTerminal
}

object Token {
  sealed trait RuleToken[+C] extends Token

  trait TerminalToken extends Token {
    override val isTerminal: Boolean = true
  }

  trait RuleTerminalToken[+C] extends TerminalToken with RuleToken[C] {
    def value: C
  }

  case class SimpleTerminalToken[@specialized C](value: C)
      extends RuleTerminalToken[C]

  trait NonTerminalToken extends Token with RuleToken[Nothing] {
    override val isTerminal: Boolean = false
  }

  case class SimpleNonTerminalToken(name: String) extends NonTerminalToken {
    override def toString: String = s"<$name>"
  }
}
