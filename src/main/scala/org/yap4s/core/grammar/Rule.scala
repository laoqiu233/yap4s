package org.yap4s.core.grammar

import org.yap4s.core.grammar.Token.{NonTerminalToken, RuleToken}
import org.yap4s.core.grammar.modify.GrammarModification
import org.yap4s.core.model.MatchResult
import org.yap4s.core.model.MatchResult.{
  MatchResultTree,
  ModifiedSubTree,
  SubTree,
  SubTreeBuilder,
  SubTreeModification
}
import org.yap4s.core.transform.MatchResultExtractor

sealed trait Rule[C] extends SubTreeBuilder {
  def leftHandSide: NonTerminalToken
  def rightHandSide: Seq[RuleToken[C]]
  def buildSubTree(headNode: MatchResult, tailNodes: Seq[MatchResult]): SubTree

  override def toString: String =
    s"(${this.getClass.getSimpleName}) $leftHandSide -> ${if (
        rightHandSide.isEmpty
      ) "<EPS>"
      else rightHandSide.mkString(", ")}"
}

object Rule {
  case class CannonRule[C](
      leftHandSide: NonTerminalToken,
      rightHandSide: Seq[RuleToken[C]],
      extractor: MatchResultExtractor[Any]
  ) extends Rule[C] {
    override def buildSubTree(
        headNode: MatchResult,
        tailNodes: Seq[MatchResult]
    ): SubTree =
      MatchResultTree(
        leftHandSide,
        headNode,
        tailNodes,
        extractor
      )
  }

  abstract class ModifiedRule[C](
      val modification: GrammarModification,
      val parentSubTreeBuilder: Rule[C]
  ) extends Rule[C]
      with SubTreeModification {
    override def buildSubTree(
        headNode: MatchResult,
        tailNodes: Seq[MatchResult]
    ): SubTree =
      ModifiedSubTree(
        leftHandSide,
        headNode,
        tailNodes,
        this
      )
  }
}
