package org.yap4s.core.transform

import org.yap4s.core.model.MatchResult.MatchResultTree

class MappedExtractor[T, V, C](
    baseTransformer: MatchResultExtractor[T, C],
    func: T => V
) extends MatchResultExtractor[V, C] {
  override def transform[C1 <: C](result: MatchResultTree[C1]): V =
    func(baseTransformer.transform(result))
}
