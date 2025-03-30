package org.yap4s.core.parse

import org.yap4s.core.model.MatchResult.SubTree

trait Parser[-C] {
  def produceRawParseTrees(tokens: Iterable[C]): Seq[SubTree]
}
