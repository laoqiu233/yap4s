package org.yap4s.core.transform

import org.yap4s.core.model.MatchResult
import org.yap4s.core.model.MatchResult.MatchResultTree

trait MatchResultNodesExtractor[+T] extends MatchResultExtractor[T] {
  override def transform(result: MatchResultTree): T =
    transformNodes(result.nodes)

  def transformNodes(nodes: Seq[MatchResult]): T
}