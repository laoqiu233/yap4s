package org.yap4s.core.transform

import org.yap4s.core.model.MatchResult.MatchResultTree

class MappedExtractor[T, V](baseTransformer: MatchResultExtractor[T], func: T => V) extends MatchResultExtractor[V] {
  override def transform(result: MatchResultTree): V =
    func(baseTransformer.transform(result))
}