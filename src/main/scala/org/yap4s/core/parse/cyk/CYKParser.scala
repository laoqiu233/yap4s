package org.yap4s.core.parse.cyk

import org.yap4s.core.grammar.{Grammar, TerminalTokenSupport, Token}
import org.yap4s.core.model.MatchResult
import org.yap4s.core.model.MatchResult.{MatchResultNode, SubTree}
import org.yap4s.core.parse.Parser
import org.yap4s.core.parse.cyk.CYKParser.{CykTable, CykTableCell}

class CYKParser[C](grammar: Grammar[_, C])(implicit
    ev: TerminalTokenSupport[C]
) extends Parser[C] {
  override def produceRawParseTrees(tokens: Iterable[C]): Seq[SubTree[C]] = {
    val finalTable = tokens.zipWithIndex.foldLeft[CykTable[C]](Nil) {
      case (table, (token, index)) =>
        parseToken(table, token, index)
    }

    for {
      lastDiagonal <- finalTable.lastOption.toSeq
      lastCell <- lastDiagonal.lastOption.toSeq
      matchedDerivations <- lastCell.matchedTokens.getOrElse(
        grammar.startToken,
        Nil
      )
    } yield {
      matchedDerivations match {
        case result: SubTree[C] => result
        case other => throw new IllegalArgumentException(s"WTF $other")
      }
    }
  }

  private def recognizeUnitRules(
      map: Map[Token, Seq[MatchResult[C]]]
  ): Map[Token, Seq[MatchResult[C]]] = {
    val newMatches = grammar.rules
      .flatMap { rule =>
        rule.rightHandSide match {
          case a :: Nil if a.isTerminal =>
            map.getOrElse(a, Nil).map { result =>
              rule.buildSubTree(result, Nil)
            }
          case _ => Nil
        }
      }
      .groupBy(_.nonTerminal)

    map ++ newMatches.map { case (k, v) =>
      k -> (map.getOrElse(k, Nil) ++ v)
    }
  }

  private def parseToken(
      table: CykTable[C],
      token: C,
      index: Int
  ): CykTable[C] = {
    val wrappedToken = ev.wrap(token)
    val newTokenMap = recognizeUnitRules(
      Map(wrappedToken -> Seq(MatchResultNode(token, index)))
    )
    val diagonal = Seq(CykTableCell(newTokenMap))

    val newDiagonal = (2 to index + 1).foldLeft(diagonal) {
      case (diagonal, length) =>
        val tokenToResult = (for {
          partition <- 1 until length
          matchedToken <- grammar.rules.flatMap { rule =>
            rule.rightHandSide match {
              case a :: b :: Nil =>
                val leftPartitionCell =
                  table(index - length + partition)(partition - 1)
                val rightPartitionCell = diagonal(length - partition - 1)

                for {
                  leftMatchResult <- leftPartitionCell.matchedTokens.getOrElse(
                    a,
                    Nil
                  )
                  rightMatchResult <- rightPartitionCell.matchedTokens
                    .getOrElse(b, Nil)
                } yield rule.buildSubTree(
                  leftMatchResult,
                  Seq(rightMatchResult)
                )
              case _ => Nil
            }

          }
        } yield matchedToken).groupBy[Token](_.nonTerminal)

        diagonal :+ CykTableCell(tokenToResult)
    }

    table :+ newDiagonal
  }
}

object CYKParser {
  type CykTable[C] = Seq[Seq[CykTableCell[C]]]

  case class CykTableCell[C](matchedTokens: Map[Token, Seq[MatchResult[C]]])
}
