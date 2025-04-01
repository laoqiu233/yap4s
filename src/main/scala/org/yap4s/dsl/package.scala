package org.yap4s

import org.yap4s.core.grammar.TerminalTokenSupport
import org.yap4s.core.grammar.Token.{RuleTerminalToken, SimpleTerminalToken}
import org.yap4s.core.transform.NonTerminalTokenExtractor
import org.yap4s.dsl.NonTerminalTokenIdentifier.{
  ExtractableNonTerminalToken,
  FictiveNonTerminalToken
}
import org.yap4s.dsl.definitions.RightHandSideDefinition.{
  MultipleRightHandSideDefinition,
  PlainDefinition0,
  PlainDefinition1,
  PlainRightHandSideDefinition
}

import scala.language.implicitConversions
import scala.reflect.ClassTag

package object dsl {
  val Empty: PlainRightHandSideDefinition[Any, Nothing] =
    PlainDefinition0[Nothing](Nil)

  @inline implicit def nonTerminalTokenIdentifierIsRightHandSide[T: ClassTag](
      token: NonTerminalTokenIdentifier[T]
  ): PlainDefinition1[T, Nothing] =
    PlainDefinition1(
      Seq(token.token),
      new NonTerminalTokenExtractor[T](0, token.token)
    )

  @inline implicit def fictiveNonTerminalIsRightHandSide(
      token: FictiveNonTerminalToken
  ): PlainDefinition0[Nothing] =
    PlainDefinition0(Seq(token.token))

  @inline implicit def iterableOfSingularsIsMultiple[T, C](
      definitions: Iterable[PlainRightHandSideDefinition[T, C]]
  ): MultipleRightHandSideDefinition[T, C] =
    MultipleRightHandSideDefinition(definitions.toSeq)

  implicit object StringIsTerminalToken extends TerminalTokenSupport[String] {
    override def wrap(t: String): RuleTerminalToken[String] =
      SimpleTerminalToken(t)
  }

  implicit object CharIsTerminalToken extends TerminalTokenSupport[Char] {
    override def wrap(t: Char): RuleTerminalToken[Char] = SimpleTerminalToken(t)
  }

  implicit class TerminalTokenOps[T](val token: T) extends AnyVal {
    def unary_!(implicit ev: TerminalTokenSupport[T]): PlainDefinition1[T, T] =
      PlainDefinition1(Seq(ev.wrap(token)), _ => token)
  }
}
