package org.yap4s.core.model

import org.yap4s.core.grammar.Token.{NonTerminalToken, TerminalToken}
import org.yap4s.core.grammar.modify.GrammarModification
import org.yap4s.core.transform.MatchResultExtractor

sealed trait MatchResult[+C] {
  def startIndex: Int

  def endIndex: Int
}

object MatchResult {
  trait Node[+C] extends MatchResult[C] {
    def index: Int
    override def startIndex: Int = index
    override def endIndex: Int = index
  }

  case class MatchResultNode[+C](terminal: C, index: Int) extends Node[C]
  case class EmptyNode(index: Int) extends Node[Nothing]

  sealed trait SubTree[C] extends MatchResult[C] {
    def nonTerminal: NonTerminalToken
    def headNode: MatchResult[C]
    def tailNodes: Seq[MatchResult[C]]
    def rebuildWithChildren(
        headNode: MatchResult[C],
        tailNodes: Seq[MatchResult[C]]
    ): SubTree[C]

    val nodes: Seq[MatchResult[C]] = headNode +: tailNodes

    override def startIndex: Int = headNode.startIndex
    override def endIndex: Int =
      tailNodes.lastOption.getOrElse(headNode).endIndex
  }

  trait SubTreeBuilder[C] {
    def buildSubTree(
        headNode: MatchResult[C],
        tailNodes: Seq[MatchResult[C]]
    ): SubTree[C]
  }

  trait SubTreeModification[C] {
    def modification: GrammarModification

    // Builder for the original rule that was modified
    def parentSubTreeBuilder: SubTreeBuilder[C]

    // Takes the nodes matched for current rule and transform to nodes that would have been matched by the parent rule
    def reverseSubTreeModification(
        headNode: MatchResult[C],
        tailNodes: Seq[MatchResult[C]]
    ): (MatchResult[C], Seq[MatchResult[C]])
  }

  case class ModifiedSubTree[C](
      nonTerminal: NonTerminalToken,
      headNode: MatchResult[C],
      tailNodes: Seq[MatchResult[C]],
      appliedModification: SubTreeModification[C]
  ) extends SubTree[C] {
    override def rebuildWithChildren(
        headNode: MatchResult[C],
        tailNodes: Seq[MatchResult[C]]
    ): SubTree[C] =
      copy(headNode = headNode, tailNodes = tailNodes)
  }

  case class MatchResultTree[C](
      nonTerminal: NonTerminalToken,
      headNode: MatchResult[C],
      tailNodes: Seq[MatchResult[C]],
      resultTransform: MatchResultExtractor[Any, C]
  ) extends SubTree[C] {

    def transformSelf: Any =
      resultTransform.transform(this)

    override def rebuildWithChildren(
        headNode: MatchResult[C],
        tailNodes: Seq[MatchResult[C]]
    ): SubTree[C] =
      new MatchResultTree(nonTerminal, headNode, tailNodes, resultTransform)
  }
}
