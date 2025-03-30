package org.yap4s.dsl

import org.yap4s.core.grammar.Rule.CannonRule
import org.yap4s.core.grammar.modify.GrammarModifier
import org.yap4s.core.grammar.{Grammar, Rule, TerminalTokenSupport}
import org.yap4s.dsl.NonTerminalTokenIdentifier.{ExtractableNonTerminalToken, FictiveNonTerminalToken}
import org.yap4s.dsl.definitions.RightHandSideDefinition
import org.yap4s.dsl.definitions.RightHandSideDefinition.{MultipleRightHandSideDefinition, PlainRightHandSideDefinition}

import scala.collection.mutable

class BuildGrammar[C: TerminalTokenSupport] {
  case class RuleBuilder[T, I <: NonTerminalTokenIdentifier[T]](grammarBuilder: GrammarBuilder, nonTerminalToken: I) {
    def produces(rightHandSideDefinition: RightHandSideDefinition[T, C]): I = {
      rightHandSideDefinition match {
        case definition: PlainRightHandSideDefinition[T, C] =>
          handleSingularDefinition(definition)
        case MultipleRightHandSideDefinition(definitions) =>
          definitions.foreach(handleSingularDefinition)
      }
      nonTerminalToken
    }

    private def handleSingularDefinition(definition: PlainRightHandSideDefinition[T, C]): Unit = {
      grammarBuilder.addRule(CannonRule(nonTerminalToken.token, definition.tokens, definition.resultTransformer))
    }

    def :=(rightHandSideDefinition: RightHandSideDefinition[T, C]): I =
      produces(rightHandSideDefinition)
  }

  class GrammarBuilder {
    private val rules: mutable.ListBuffer[Rule[C]] = mutable.ListBuffer.empty

    def rule(nonTerminalToken: FictiveNonTerminalToken): RuleBuilder[Any, FictiveNonTerminalToken] =
      RuleBuilder(this, nonTerminalToken)

    def rule[T](extractableNonTerminalToken: ExtractableNonTerminalToken[T]): RuleBuilder[T, ExtractableNonTerminalToken[T]] =
      RuleBuilder(this, extractableNonTerminalToken)

    def addRule(rule: Rule[C]): GrammarBuilder = {
      rules += rule
      this
    }

    def fragment(name: String): FictiveNonTerminalToken =
      FictiveNonTerminalToken(name)

    def parses[T](name: String): ExtractableNonTerminalToken[T] =
      ExtractableNonTerminalToken(name)

    def startsWith[T](nonTerminalToken: NonTerminalTokenIdentifier[T]): GrammarFinalizer[T] =
      new GrammarFinalizer[T](rules.toSeq, nonTerminalToken, Nil)
  }

  case class GrammarFinalizer[T](rules: Seq[Rule[C]], startToken: NonTerminalTokenIdentifier[T], modifications: Seq[GrammarModifier]) {
    def modifyWith(modifier: GrammarModifier): GrammarFinalizer[T] =
      copy(modifications = modifications :+ modifier)
  }

  def using[T](build: GrammarBuilder => GrammarFinalizer[T]): Grammar[T, C] = {
    val builder = new GrammarBuilder
    val finalizer = build(builder)
    val grammar = new Grammar[T, C](finalizer.rules, finalizer.startToken.token, finalizer.modifications.map(_.modificationLabel))

    finalizer.modifications.foldLeft(grammar) { (grammar, modifier) =>
      modifier.modifyGrammar(grammar)
    }
  }
}

object BuildGrammar {
  def apply[T: TerminalTokenSupport]: BuildGrammar[T] = new BuildGrammar[T]
}
