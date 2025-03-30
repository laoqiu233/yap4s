package org.yap4s.core.transform

import org.yap4s.core.model.MatchResult
import org.yap4s.core.model.MatchResult.MatchResultTree

abstract class PreProcessNodesExtractor[T](
    baseTransformer: MatchResultExtractor[T]
) extends MatchResultExtractor[T] {
  def preprocess(
      head: MatchResult,
      tail: Seq[MatchResult]
  ): (MatchResult, Seq[MatchResult])

  override def transform(result: MatchResultTree): T = {
    val (newHead, newTail) = preprocess(result.headNode, result.tailNodes)
    baseTransformer.transform(
      result.copy(headNode = newHead, tailNodes = newTail)
    )
  }
}
