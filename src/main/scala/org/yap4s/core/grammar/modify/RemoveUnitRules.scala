package org.yap4s.core.grammar.modify
import org.yap4s.core.grammar.Rule.ModifiedRule
import org.yap4s.core.grammar.Token.NonTerminalToken
import org.yap4s.core.grammar.{Grammar, Rule, TerminalTokenSupport, Token}
import org.yap4s.core.model.MatchResult

import scala.annotation.tailrec

object RemoveUnitRules extends GrammarModifier {
  override val modificationLabel: GrammarModification =
    GrammarModification.RemoveUnitRules

  private case class AppendTransitiveNodeRule[C](
      outerRule: Rule[C],
      innerRule: Rule[C]
  ) extends ModifiedRule[C](GrammarModification.RemoveUnitRules, outerRule) {
    override val leftHandSide: NonTerminalToken = outerRule.leftHandSide
    override val rightHandSide: Seq[Token.RuleToken[C]] =
      innerRule.rightHandSide

    override def reverseSubTreeModification(
        headNode: MatchResult[C],
        tailNodes: Seq[MatchResult[C]]
    ): (MatchResult[C], Seq[MatchResult[C]]) =
      innerRule.buildSubTree(headNode, tailNodes) -> Nil
  }

  override def modifyGrammar[T, C: TerminalTokenSupport](
      grammar: Grammar[T, C]
  ): Grammar[T, C] = {
    val newRules = fixUnitRulesIterate(grammar.rules)
    grammar.copy(rules = newRules)
  }

  @tailrec
  private def fixUnitRulesIterate[T, C: TerminalTokenSupport](
      rules: Seq[Rule[C]]
  ): Seq[Rule[C]] = {
    val hasUnitRules = rules.exists { rule =>
      rule.rightHandSide match {
        case a :: Nil if a.isNonTerminal => true
        case _                           => false
      }
    }

    if (hasUnitRules)
      fixUnitRulesIterate(fixUnitRules(rules))
    else
      rules
  }

  private def fixUnitRules[T, C: TerminalTokenSupport](
      rules: Seq[Rule[C]]
  ): Seq[Rule[C]] =
    rules.flatMap { outerRule =>
      outerRule.rightHandSide match {
        case a :: Nil if a.isNonTerminal =>
          rules.filter(_.leftHandSide == a).map { innerRule =>
            AppendTransitiveNodeRule(outerRule, innerRule)
          }
        case _ => Seq(outerRule)
      }
    }
}
