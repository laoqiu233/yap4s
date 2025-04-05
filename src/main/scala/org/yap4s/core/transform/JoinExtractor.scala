package org.yap4s.core.transform

import org.yap4s.core.model.MatchResult.MatchResultTree

class JoinExtractor[+L, +R, -C](
    left: MatchResultExtractor[L, C],
    right: MatchResultExtractor[R, C]
) extends MatchResultExtractor[(L, R), C] {
  override def transform[C1 <: C](result: MatchResultTree[C1]): (L, R) =
    left.transform(result) -> right.transform(result)
}
