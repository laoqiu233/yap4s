package org.yap4s.core.transform

import org.yap4s.core.model.MatchResult.MatchResultTree

trait MatchResultExtractor[+T] {
  def transform(result: MatchResultTree): T

}
