package org.yap4s.core.transform

import org.yap4s.core.model.MatchResult.MatchResultTree

trait MatchResultExtractor[+T, -C] {
  def transform[C1 <: C](result: MatchResultTree[C1]): T
}
