package org.yap4s.dsl

import org.yap4s.core.grammar.Rule.CannonRule
import org.yap4s.core.grammar.modify.GrammarModifier
import org.yap4s.core.grammar.{Grammar, Rule, TerminalTokenSupport}
import org.yap4s.dsl.BuildGrammar.{GrammarBuilder, GrammarFinalizer}
import org.yap4s.dsl.NonTerminalTokenIdentifier.{
  ExtractableNonTerminalToken,
  FictiveNonTerminalToken
}
import org.yap4s.dsl.definitions.RightHandSideDefinition
import org.yap4s.dsl.definitions.RightHandSideDefinition.{
  MultipleRightHandSideDefinition,
  PlainRightHandSideDefinition
}

import scala.collection.mutable
import scala.reflect.ClassTag

class BuildGrammar[C: TerminalTokenSupport] {
  def using[T](
      build: GrammarBuilder[C] => GrammarFinalizer[C, T]
  ): Grammar[T, C] = {
    val builder = new GrammarBuilder[C]
    val finalizer = build(builder)
    val grammar = new Grammar[T, C](
      finalizer.rules,
      finalizer.startToken.token,
      finalizer.modifications.map(_.modificationLabel)
    )

    finalizer.modifications.foldLeft(grammar) { (grammar, modifier) =>
      modifier.modifyGrammar(grammar)
    }
  }
}

object BuildGrammar {
  def apply[T: TerminalTokenSupport]: BuildGrammar[T] = new BuildGrammar[T]

  class GrammarBuilder[C: TerminalTokenSupport] {
    private val rules: mutable.ListBuffer[Rule[C]] = mutable.ListBuffer.empty
    private val fictiveNonTerminalTokens
        : mutable.Map[String, FictiveNonTerminalToken] = mutable.Map.empty
    private val extractableNonTerminalTokens
        : mutable.Map[String, ExtractableNonTerminalToken[_]] =
      mutable.Map.empty

    def rule(
        nonTerminalToken: FictiveNonTerminalToken
    ): RuleBuilder[C, Any, FictiveNonTerminalToken] =
      RuleBuilder(this, nonTerminalToken)

    def rule[T](
        extractableNonTerminalToken: ExtractableNonTerminalToken[T]
    ): RuleBuilder[C, T, ExtractableNonTerminalToken[T]] =
      RuleBuilder(this, extractableNonTerminalToken)

    def addRule(rule: Rule[C]): GrammarBuilder[C] = {
      rules += rule
      this
    }

    def fragment(name: String): FictiveNonTerminalToken = {
      if (hasParse(name)) {
        throw new IllegalArgumentException(
          s"$name is already defined as a parse token"
        )
      } else {
        fictiveNonTerminalTokens.get(name) match {
          case Some(value) => value
          case None =>
            val newToken = FictiveNonTerminalToken(name)
            fictiveNonTerminalTokens(name) = newToken
            newToken
        }
      }
    }

    def hasFragment(name: String): Boolean =
      fictiveNonTerminalTokens.contains(name)

    def parses[T](
        name: String
    )(implicit tag: ClassTag[T]): ExtractableNonTerminalToken[T] = {
      if (hasFragment(name)) {
        throw new IllegalArgumentException(
          s"$name is already defined as a fragment token"
        )
      } else {
        extractableNonTerminalTokens.get(name) match {
          case Some(value) =>
            if (value.tag != tag)
              throw new IllegalArgumentException(
                s"Redefining token $name extracted type, expected ${value.tag.runtimeClass.getSimpleName}"
              )
            else
              value.asInstanceOf[ExtractableNonTerminalToken[T]]
          case None =>
            val newToken = ExtractableNonTerminalToken[T](name)
            extractableNonTerminalTokens(name) = newToken
            newToken
        }
      }
    }

    def hasParse(name: String): Boolean =
      extractableNonTerminalTokens.contains(name)

    def startsWith[T](
        nonTerminalToken: NonTerminalTokenIdentifier[T]
    ): GrammarFinalizer[C, T] =
      new GrammarFinalizer[C, T](rules.toSeq, nonTerminalToken, Nil)
  }

  case class RuleBuilder[C, T, I <: NonTerminalTokenIdentifier[T]](
      grammarBuilder: GrammarBuilder[C],
      nonTerminalToken: I
  ) {
    def produces(rightHandSideDefinition: RightHandSideDefinition[T, C]): I = {
      rightHandSideDefinition match {
        case definition: PlainRightHandSideDefinition[T, C] =>
          handleSingularDefinition(definition)
        case MultipleRightHandSideDefinition(definitions) =>
          definitions.foreach(handleSingularDefinition)
      }
      nonTerminalToken
    }

    private def handleSingularDefinition(
        definition: PlainRightHandSideDefinition[T, C]
    ): Unit = {
      grammarBuilder.addRule(
        CannonRule(
          nonTerminalToken.token,
          definition.tokens,
          definition.resultTransformer
        )
      )
    }

    def :=(rightHandSideDefinition: RightHandSideDefinition[T, C]): I =
      produces(rightHandSideDefinition)
  }

  case class GrammarFinalizer[C, T](
      rules: Seq[Rule[C]],
      startToken: NonTerminalTokenIdentifier[T],
      modifications: Seq[GrammarModifier]
  ) {
    def modifyWith(modifier: GrammarModifier): GrammarFinalizer[C, T] =
      copy(modifications = modifications :+ modifier)
  }
}
