package org.yap4s.core.transform
import org.yap4s.core.model.MatchResult
import org.yap4s.core.model.MatchResult.MatchResultTree

class AllSubNodesExtractor[T] extends MatchResultExtractor[Seq[T], Any] {
  override def transform[C1 <: Any](
      result: MatchResult.MatchResultTree[C1]
  ): Seq[T] =
    result.nodes.collect { case resultTree: MatchResultTree[C1] =>
      resultTree.transformSelf.asInstanceOf[T]
    }
}
