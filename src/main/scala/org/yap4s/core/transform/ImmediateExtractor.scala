package org.yap4s.core.transform
import org.yap4s.core.model.MatchResult

class ImmediateExtractor[V](value: => V) extends MatchResultExtractor[V, Any] {
  override def transform[C1 <: Any](
      result: MatchResult.MatchResultTree[C1]
  ): V =
    value
}
