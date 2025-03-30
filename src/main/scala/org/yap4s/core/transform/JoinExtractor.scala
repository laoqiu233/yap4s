package org.yap4s.core.transform

import org.yap4s.core.model.MatchResult.MatchResultTree

class JoinExtractor[L, R](left: MatchResultExtractor[L], right: MatchResultExtractor[R]) extends MatchResultExtractor[(L, R)] {
  override def transform(result: MatchResultTree): (L, R) =
    left.transform(result) -> right.transform(result)
}
