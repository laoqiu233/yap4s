package org.yap4s.core.grammar.modify
import org.yap4s.core.grammar.Rule.{CannonRule, ModifiedRule}
import org.yap4s.core.grammar.Token.{
  NonTerminalToken,
  RuleToken,
  SimpleNonTerminalToken
}
import org.yap4s.core.grammar.{Grammar, Rule, TerminalTokenSupport}
import org.yap4s.core.model.MatchResult
import org.yap4s.core.model.MatchResult.{ModifiedSubTree, SubTree}
import org.yap4s.core.transform.ImmediateExtractor

import scala.annotation.tailrec

object ChomskyNormalForm extends GrammarModifier {
  override val modificationLabel: GrammarModification =
    GrammarModification.ChomskyNormalForm

  private case class ChomskyRootRule[C](
      leftHandSide: NonTerminalToken,
      a: RuleToken[C],
      b: RuleToken[C],
      parentRule: Rule[C]
  ) extends ModifiedRule[C](GrammarModification.ChomskyNormalForm, parentRule) {
    private val unfoldCount = parentRule.rightHandSide.size - 2

    override val rightHandSide: Seq[RuleToken[C]] = Seq(a, b)

    @tailrec
    private def unfoldChomsky(
        currHead: Option[MatchResult[C]],
        unfoldsLeft: Int = unfoldCount,
        acc: Seq[MatchResult[C]] = Nil
    ): Seq[MatchResult[C]] =
      currHead match {
        case Some(value: SubTree[C]) if unfoldsLeft > 0 =>
          unfoldChomsky(
            value.tailNodes.headOption,
            unfoldsLeft - 1,
            acc :+ value.headNode
          )
        case Some(value) => acc :+ value
        case None        => acc
      }

    override def reverseSubTreeModification(
        headNode: MatchResult[C],
        tailNodes: Seq[MatchResult[C]]
    ): (MatchResult[C], Seq[MatchResult[C]]) =
      headNode -> unfoldChomsky(tailNodes.headOption)
  }

  override def modifyGrammar[T, C: TerminalTokenSupport](
      grammar: Grammar[T, C]
  ): Grammar[T, C] = {
    val newRules = grammar.rules.flatMap { rule =>
      rule.rightHandSide match {
        case tokens if tokens.size > 2 =>
          val contRules = convertToChomskyNormalForm[C](
            rule.leftHandSide,
            tokens,
            s"${rule.leftHandSide}_cnf_${rule.hashCode()}"
          )
          val firstCont = contRules.head.rightHandSide
          val rootRule = ChomskyRootRule(
            rule.leftHandSide,
            firstCont.head,
            firstCont.tail.head,
            rule
          )
          rootRule +: contRules.tail
        case _ => Seq(rule)
      }
    }
    grammar.copy(rules = newRules)
  }

  @tailrec
  private def convertToChomskyNormalForm[C](
      leftHandSide: NonTerminalToken,
      tokens: Seq[RuleToken[C]],
      tokensPrefix: String,
      acc: Seq[Rule[C]] = Nil
  ): Seq[Rule[C]] = {

    tokens match {
      case a :: b :: Nil =>
        acc :+ CannonRule[C](
          leftHandSide,
          Seq(a, b),
          new ImmediateExtractor(())
        )
      case a :: tail =>
        val newNonTerminalToken = SimpleNonTerminalToken(
          s"${tokensPrefix}_${acc.size}"
        )

        convertToChomskyNormalForm[C](
          newNonTerminalToken,
          tail,
          tokensPrefix,
          acc :+ CannonRule[C](
            leftHandSide,
            Seq(a, newNonTerminalToken),
            new ImmediateExtractor(())
          )
        )
      case Nil => acc
    }
  }
}
