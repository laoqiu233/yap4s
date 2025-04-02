package org.yap4s.dsl

import org.yap4s.core.grammar.Token.{NonTerminalToken, SimpleNonTerminalToken}
import org.yap4s.dsl.BuildGrammar.GrammarBuilder

import scala.reflect.ClassTag

sealed trait NonTerminalTokenIdentifier[T] {
  def name: String
  val token: NonTerminalToken = SimpleNonTerminalToken(name)
}

object NonTerminalTokenIdentifier {
  trait EmptyExtractionProvider[+T] {
    def emptyValue: T
  }

  implicit object EmptyListExtractionProvider
      extends EmptyExtractionProvider[Seq[Nothing]] {
    override def emptyValue: Seq[Nothing] = Nil
  }

  implicit object EmptyStringExtractionProvider
      extends EmptyExtractionProvider[String] {
    override def emptyValue: String = ""
  }

  case class ExtractableNonTerminalToken[T](name: String)(implicit
      val tag: ClassTag[T]
  ) extends NonTerminalTokenIdentifier[T] {
    def ?(implicit
        grammar: GrammarBuilder[_],
        empty: EmptyExtractionProvider[T]
    ): ExtractableNonTerminalToken[T] = {
      val orEmptyFragmentName = s"$$${name}_or_empty"
      if (grammar.hasParse(orEmptyFragmentName))
        grammar parses orEmptyFragmentName
      else {
        grammar rule (grammar parses [T] orEmptyFragmentName) := this || (Empty as empty.emptyValue)
      }
    }
  }

  case class FictiveNonTerminalToken(name: String)
      extends NonTerminalTokenIdentifier[Any] {
    def ?(implicit grammar: GrammarBuilder[_]): FictiveNonTerminalToken = {
      val orEmptyFragmentName = s"$$${name}_or_empty"
      if (grammar.hasFragment(orEmptyFragmentName))
        grammar fragment orEmptyFragmentName
      else {
        grammar rule (grammar fragment orEmptyFragmentName) := this || Empty
      }
    }
  }
}
