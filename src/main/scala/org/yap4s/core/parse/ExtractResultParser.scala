package org.yap4s.core.parse

import org.yap4s.core.model.MatchResult.{MatchResultTree, SubTree}

import scala.util.Try

trait ExtractResultParser[+T, -C] extends Parser[C] {
  private def extractResult(resultTree: SubTree): Option[T] =
    resultTree match {
      case result: MatchResultTree =>
        Try(result.transformSelf.asInstanceOf[T]).toOption
    }

  def produceResults(tokens: Iterable[C]): Seq[T] =
    produceRawParseTrees(tokens).flatMap(extractResult)
}
