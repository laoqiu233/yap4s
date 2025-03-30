package org.yap4s.core.grammar.modify
import org.yap4s.core.grammar.Rule.ModifiedRule
import org.yap4s.core.grammar.Token.NonTerminalToken
import org.yap4s.core.grammar.{Grammar, Rule, TerminalTokenSupport, Token}
import org.yap4s.core.model.MatchResult

object RemoveUnitRules extends GrammarModifier {
  override val modificationLabel: GrammarModification = GrammarModification.RemoveUnitRules

  private case class AppendTransitiveNodeRule[C](outerRule: Rule[C], innerRule: Rule[C]) extends ModifiedRule[C](GrammarModification.RemoveUnitRules, outerRule) {
    override val leftHandSide: NonTerminalToken = outerRule.leftHandSide
    override val rightHandSide: Seq[Token.RuleToken[C]] = innerRule.rightHandSide

    override def reverseSubTreeModification(headNode: MatchResult, tailNodes: Seq[MatchResult]): (MatchResult, Seq[MatchResult]) =
      innerRule.buildSubTree(headNode, tailNodes) -> Nil
  }

  override def modifyGrammar[T, C: TerminalTokenSupport](grammar: Grammar[T, C]): Grammar[T, C] = {
    val newRules = grammar.rules.flatMap { outerRule =>
      outerRule.rightHandSide match {
        case a :: Nil if a.isNonTerminal =>
          grammar.rules.filter(_.leftHandSide == a).map { innerRule =>
            AppendTransitiveNodeRule(outerRule, innerRule)
          }
        case _ => Seq(outerRule)
      }
    }

    grammar.copy(rules = newRules)
  }
}