package org.yap4s.dsl.definitions

import org.yap4s.core.grammar.TerminalTokenSupport
import org.yap4s.core.grammar.Token.{RuleTerminalToken, RuleToken}
import org.yap4s.core.transform.{
  ImmediateExtractor,
  JoinExtractor,
  MappedExtractor,
  MatchResultExtractor,
  NonTerminalTokenExtractor,
  TerminalTokenExtractor
}
import org.yap4s.dsl.NonTerminalTokenIdentifier.{
  ExtractableNonTerminalToken,
  FictiveNonTerminalToken
}

sealed trait RightHandSideDefinition[+T, -C]

object RightHandSideDefinition {
  trait PlainRightHandSideDefinition[+T, -C]
      extends RightHandSideDefinition[T, C] {
    type Self[-C0] <: PlainRightHandSideDefinition[T, C0]
    type NextOutput[T0]
    type Next[T0, -C0] <: PlainRightHandSideDefinition[NextOutput[T0], C0]

    def tokens: Seq[RuleToken[C]]
    def resultTransformer: MatchResultExtractor[T, C]

    protected def nextExtract[T1, C1 <: C](
        extractor: MatchResultExtractor[T1, C1]
    ): Next[T1, C1]

    protected def copyWithNewToken[C1 <: C](token: RuleToken[C1]): Self[C1]

    def ++[T1](other: ExtractableNonTerminalToken[T1]): Self[C]#Next[T1, C] =
      copyWithNewToken(other.token).nextExtract(
        new NonTerminalTokenExtractor[T1](tokens.size, other.token)
      )

    def ++(other: FictiveNonTerminalToken): Self[C] =
      copyWithNewToken(other.token)

    def ++[C1 <: C](other: C1)(implicit
        ev: TerminalTokenSupport[C1]
    ): Self[C1] =
      this ++ ev.wrap(other)

    def ++[C1 <: C](other: RuleTerminalToken[C1]): Self[C1] =
      copyWithNewToken(other)

    def ++![C1 <: C](other: C1)(implicit
        ev: TerminalTokenSupport[C1]
    ): Self[C1]#Next[C1, C1] =
      this ++! ev.wrap(other)

    def ++![C1 <: C](other: RuleTerminalToken[C1]): Self[C1]#Next[C1, C1] =
      copyWithNewToken(other).nextExtract(
        new TerminalTokenExtractor[C1](tokens.size, other)
      )

    def ||[T1 >: T, C1 <: C](
        other: PlainRightHandSideDefinition[T1, C1]
    ): MultipleRightHandSideDefinition[T1, C1] =
      MultipleRightHandSideDefinition(Seq(this, other))

    def map[V](func: T => V): PlainDefinition1[V, C] =
      PlainDefinition1(tokens, new MappedExtractor(resultTransformer, func))

    def mapSome: PlainRightHandSideDefinition[Option[T], C] =
      map(Some.apply)

    def unary_~ : PlainRightHandSideDefinition[Option[T], C] =
      mapSome

    def as[V](value: V): PlainDefinition1[V, C] =
      PlainDefinition1(tokens, new ImmediateExtractor(value))
  }

  case class MultipleRightHandSideDefinition[+T, C](
      definitions: Seq[PlainRightHandSideDefinition[T, C]]
  ) extends RightHandSideDefinition[T, C] {
    def ||[T1 >: T, C1 <: C](
        other: PlainRightHandSideDefinition[T1, C1]
    ): MultipleRightHandSideDefinition[T1, C1] =
      copy(definitions :+ other)
  }

  case class PlainDefinition0[-Terminal](tokens: Seq[RuleToken[Terminal]])
      extends PlainRightHandSideDefinition[Any, Terminal] {
    type Self[-C0] = PlainDefinition0[C0]
    type NextOutput[T0] = T0
    type Next[T0, -C0] = PlainDefinition1[T0, C0]

    override val resultTransformer: MatchResultExtractor[Nothing, Terminal] =
      new ImmediateExtractor[Nothing](
        throw new Exception("Tried to extract from fragment")
      )

    override protected def nextExtract[T1, C1 <: Terminal](
        extractor: MatchResultExtractor[T1, C1]
    ): PlainDefinition1[T1, C1] =
      PlainDefinition1(tokens, extractor)

    override protected def copyWithNewToken[C1 <: Terminal](
        token: RuleToken[C1]
    ): PlainDefinition0[C1] = copy(tokens :+ token)
  }

  case class PlainDefinition1[A, -Terminal](
      tokens: Seq[RuleToken[Terminal]],
      resultTransformer: MatchResultExtractor[A, Terminal]
  ) extends PlainRightHandSideDefinition[A, Terminal] {
    override type Self[-C0] = PlainDefinition1[A, C0]
    override type NextOutput[T0] = (A, T0)
    override type Next[T0, -C0] = PlainDefinition2[A, T0, C0]

    override protected def nextExtract[T1, C1 <: Terminal](
        extractor: MatchResultExtractor[T1, C1]
    ): PlainDefinition2[A, T1, C1] =
      PlainDefinition2(tokens, resultTransformer, extractor)

    override protected def copyWithNewToken[C1 <: Terminal](
        token: RuleToken[C1]
    ): PlainDefinition1[A, C1] =
      copy(tokens :+ token)
  }

  case class PlainDefinition2[A, B, -Terminal](
      tokens: Seq[RuleToken[Terminal]],
      a: MatchResultExtractor[A, Terminal],
      b: MatchResultExtractor[B, Terminal]
  ) extends PlainRightHandSideDefinition[(A, B), Terminal] {
    override type Self[-C0] = PlainDefinition2[A, B, C0]
    override type NextOutput[T0] = ((A, B), T0)
    override type Next[T0, -C0] = PlainDefinition2[(A, B), T0, C0]

    override val resultTransformer: MatchResultExtractor[(A, B), Terminal] =
      new JoinExtractor(a, b)

    override protected def nextExtract[T1, C1 <: Terminal](
        extractor: MatchResultExtractor[T1, C1]
    ): PlainDefinition2[(A, B), T1, C1] =
      PlainDefinition2(tokens, resultTransformer, extractor)

    override protected def copyWithNewToken[C1 <: Terminal](
        token: RuleToken[C1]
    ): PlainDefinition2[A, B, C1] =
      copy(tokens :+ token)
  }
}
