package org.yap4s.core.transform

import org.yap4s.core.grammar.Token.NonTerminalToken
import org.yap4s.core.model.MatchResult
import org.yap4s.core.model.MatchResult.MatchResultTree

class NonTerminalTokenExtractor[T](
    tokenPosition: Int,
    expectedNonTerminal: NonTerminalToken
) extends MatchResultNodesExtractor[T, Any] {
  override def transformNodes(nodes: Seq[MatchResult[Any]]): T =
    nodes.lift(tokenPosition) match {
      case Some(tree: MatchResultTree[Any]) =>
        val tempResult = tree.transformSelf
        tempResult.asInstanceOf[T]
      case Some(value) =>
        throw new IllegalArgumentException(
          s"Expected non-terminal token $expectedNonTerminal at position $tokenPosition, got $value"
        )
      case None =>
        throw new IllegalArgumentException(
          s"Expected at least $tokenPosition nodes in subtree, got ${nodes.size}"
        )
    }
}
