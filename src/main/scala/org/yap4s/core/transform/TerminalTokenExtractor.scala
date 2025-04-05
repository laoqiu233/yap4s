package org.yap4s.core.transform

import org.yap4s.core.grammar.Token.{NonTerminalToken, TerminalToken}
import org.yap4s.core.model.MatchResult
import org.yap4s.core.model.MatchResult.{MatchResultNode, MatchResultTree}

class TerminalTokenExtractor[C](
    tokenPosition: Int,
    expectedTerminal: TerminalToken
) extends MatchResultNodesExtractor[C, C] {
  override def transformNodes(nodes: Seq[MatchResult[C]]): C =
    nodes.lift(tokenPosition) match {
      case Some(node: MatchResultNode[C]) =>
        node.terminal
      case Some(value) =>
        throw new IllegalArgumentException(
          s"Expected non-terminal token $expectedTerminal at position $tokenPosition, got $value"
        )
      case None =>
        throw new IllegalArgumentException(
          s"Expected at least $tokenPosition nodes in subtree, got ${nodes.size}"
        )
    }
}
