package org.yap4s.core.grammar.modify
import org.yap4s.core.grammar.Rule.ModifiedRule
import org.yap4s.core.grammar.Token.{NonTerminalToken, RuleToken}
import org.yap4s.core.grammar.{Grammar, Rule, TerminalTokenSupport, Token}
import org.yap4s.core.model.MatchResult
import org.yap4s.core.model.MatchResult.EmptyNode

import scala.annotation.tailrec

object RemoveEmptyProductions extends GrammarModifier {
  override val modificationLabel: GrammarModification =
    GrammarModification.RemoveEmptyProductions

  private case class InsertEmptyNodesRule[C](
      parentRule: Rule[C],
      emptyRule: Rule[C],
      removedPositions: Seq[Int]
  ) extends ModifiedRule[C](
        GrammarModification.RemoveEmptyProductions,
        parentRule
      ) {
    override val leftHandSide: NonTerminalToken = parentRule.leftHandSide

    override val rightHandSide: Seq[RuleToken[C]] =
      removeAt(parentRule.rightHandSide, removedPositions)

    private val insertPositions = removePosToInsertPos(removedPositions)

    private val startsWithEmptyProduction =
      insertPositions.headOption.fold(false)(_ == 0)

    private def buildEmptyFromPrev(prev: MatchResult[C]): MatchResult[C] =
      emptyRule.buildSubTree(
        EmptyNode(prev.endIndex),
        Nil
      )

    override def reverseSubTreeModification(
        headNode: MatchResult[C],
        tailNodes: Seq[MatchResult[C]]
    ): (MatchResult[C], Seq[MatchResult[C]]) =
      if (startsWithEmptyProduction) {
        val newHead = emptyRule.buildSubTree(
          EmptyNode(headNode.startIndex),
          Nil
        )
        newHead -> insertAt(
          newHead,
          headNode +: tailNodes,
          insertPositions.tail,
          buildEmptyFromPrev
        )
      } else
        headNode -> insertAt(
          headNode,
          tailNodes,
          insertPositions,
          buildEmptyFromPrev,
          currPos = 1
        )
  }

  @tailrec
  override def modifyGrammar[T, C: TerminalTokenSupport](
      grammar: Grammar[T, C]
  ): Grammar[T, C] = {
    val emptyProduction = grammar.rules.find(_.rightHandSide.isEmpty)

    emptyProduction match {
      case Some(emptyRule) =>
        val emptyNonTerminal = emptyRule.leftHandSide
        val newRules = grammar.rules.filterNot(_ == emptyRule).flatMap { rule =>
          if (rule.rightHandSide.contains(emptyNonTerminal))
            findEmptyPositionVariants(rule.rightHandSide, emptyNonTerminal)
              .map { emptyIndexes =>
                if (emptyIndexes.isEmpty)
                  rule
                else
                  InsertEmptyNodesRule(rule, emptyRule, emptyIndexes)
              }
          else
            Seq(rule)
        }

        val newGrammar = grammar.copy[T, C](rules = newRules)

        modifyGrammar(newGrammar)
      case None => grammar
    }
  }

  @tailrec
  private def findEmptyPositionVariants[C](
      tokens: Seq[RuleToken[C]],
      tokenToRemove: Token,
      emptyPositions: Seq[Seq[Int]] = Seq(Nil),
      currIndex: Int = 0
  ): Seq[Seq[Int]] =
    tokens match {
      case Nil => emptyPositions
      case head :: tail =>
        val nextEmptyPositions = emptyPositions ++ (
          if (head == tokenToRemove)
            emptyPositions.map(_ :+ currIndex)
          else Nil
        )
        findEmptyPositionVariants(
          tail,
          tokenToRemove,
          nextEmptyPositions,
          currIndex + 1
        )
    }

  @tailrec
  private def removePosToInsertPos(
      positions: Seq[Int],
      currIndex: Int = 0,
      acc: Seq[Int] = Nil
  ): Seq[Int] =
    positions match {
      case head :: tail =>
        removePosToInsertPos(tail, currIndex + 1, acc :+ (head - currIndex))
      case Nil => acc
    }

  @tailrec
  private def removeAt[T](
      originalArr: Seq[T],
      positions: Seq[Int],
      currIndex: Int = 0,
      acc: Seq[T] = Nil
  ): Seq[T] =
    positions match {
      case nextPos :: positionsTail =>
        originalArr match {
          case head :: tail =>
            if (currIndex == nextPos)
              removeAt(tail, positionsTail, currIndex + 1, acc)
            else
              removeAt(tail, positions, currIndex + 1, acc :+ head)
          case Nil => acc
        }
      case Nil =>
        acc ++ originalArr
    }

  @tailrec
  private def insertAt[T](
      prev: T,
      originalArr: Seq[T],
      positions: Seq[Int],
      buildNewFromPrev: T => T,
      currPos: Int = 0,
      acc: Seq[T] = Nil
  ): Seq[T] =
    positions match {
      case nextPos :: positionsTail =>
        if (currPos < nextPos && originalArr.nonEmpty) {
          insertAt(
            originalArr.head,
            originalArr.tail,
            positions,
            buildNewFromPrev,
            currPos + 1,
            acc :+ originalArr.head
          )
        } else {
          val newElement = buildNewFromPrev(prev)
          insertAt(
            newElement,
            originalArr,
            positionsTail,
            buildNewFromPrev,
            currPos,
            acc :+ newElement
          )
        }
      case Nil => acc ++ originalArr
    }
}
