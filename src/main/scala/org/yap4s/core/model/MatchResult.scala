package org.yap4s.core.model

import org.yap4s.core.grammar.Token.{NonTerminalToken, TerminalToken}
import org.yap4s.core.grammar.modify.GrammarModification
import org.yap4s.core.transform.MatchResultExtractor

sealed trait MatchResult {
  def startIndex: Int

  def endIndex: Int
}

object MatchResult {
  trait Node extends MatchResult {
    def index: Int
    override def startIndex: Int = index
    override def endIndex: Int = index
  }

  case class MatchResultNode(terminal: TerminalToken, index: Int) extends Node
  case class EmptyNode(index: Int) extends Node

  sealed trait SubTree extends MatchResult {
    def nonTerminal: NonTerminalToken
    def headNode: MatchResult
    def tailNodes: Seq[MatchResult]
    def rebuildWithChildren(
        headNode: MatchResult,
        tailNodes: Seq[MatchResult]
    ): SubTree

    val nodes: Seq[MatchResult] = headNode +: tailNodes

    override def startIndex: Int = headNode.startIndex
    override def endIndex: Int =
      tailNodes.lastOption.getOrElse(headNode).endIndex
  }

  trait SubTreeBuilder {
    def buildSubTree(
        headNode: MatchResult,
        tailNodes: Seq[MatchResult]
    ): SubTree
  }

  trait SubTreeModification {
    def modification: GrammarModification

    // Builder for the original rule that was modified
    def parentSubTreeBuilder: SubTreeBuilder

    // Takes the nodes matched for current rule and transform to nodes that would have been matched by the parent rule
    def reverseSubTreeModification(
        headNode: MatchResult,
        tailNodes: Seq[MatchResult]
    ): (MatchResult, Seq[MatchResult])
  }

  case class ModifiedSubTree(
      nonTerminal: NonTerminalToken,
      headNode: MatchResult,
      tailNodes: Seq[MatchResult],
      appliedModification: SubTreeModification
  ) extends SubTree {
    override def rebuildWithChildren(
        headNode: MatchResult,
        tailNodes: Seq[MatchResult]
    ): SubTree =
      copy(headNode = headNode, tailNodes = tailNodes)
  }

  case class MatchResultTree(
      nonTerminal: NonTerminalToken,
      headNode: MatchResult,
      tailNodes: Seq[MatchResult],
      resultTransform: MatchResultExtractor[Any]
  ) extends SubTree {

    def transformSelf: Any =
      resultTransform.transform(this)

    override def rebuildWithChildren(
        headNode: MatchResult,
        tailNodes: Seq[MatchResult]
    ): SubTree =
      copy(headNode = headNode, tailNodes = tailNodes)
  }
}
