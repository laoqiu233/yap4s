package org.yap4s.core.transform

import org.yap4s.core.model.MatchResult
import org.yap4s.core.model.MatchResult.MatchResultTree

trait MatchResultNodesExtractor[+T, -C] extends MatchResultExtractor[T, C] {
  override def transform[C1 <: C](result: MatchResultTree[C1]): T =
    transformNodes(result.nodes)

  def transformNodes(nodes: Seq[MatchResult[C]]): T
}
