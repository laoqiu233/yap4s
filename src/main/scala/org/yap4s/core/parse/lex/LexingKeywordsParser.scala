package org.yap4s.core.parse.lex

import org.yap4s.core.cartesianProduct
import org.yap4s.core.grammar.Rule.CannonRule
import org.yap4s.core.grammar.Token.{
  NonTerminalToken,
  RuleTerminalToken,
  SimpleNonTerminalToken
}
import org.yap4s.core.grammar.{Grammar, Rule}
import org.yap4s.core.model.MatchResult.{EmptyNode, MatchResultNode, SubTree}
import org.yap4s.core.parse.Parser
import org.yap4s.core.transform.AllSubNodesExtractor

import scala.annotation.tailrec
import scala.collection.mutable

class LexingKeywordsParser[T, C](
    grammar: Grammar[T, C],
    intersectionTokens: Seq[RuleTerminalToken[C]]
) extends Parser[C] {
  val keywordsNonTerminalToken = SimpleNonTerminalToken("keywords")
  val keywordsRule =
    CannonRule[C](keywordsNonTerminalToken, Nil, new AllSubNodesExtractor[T])

  private case class Transition(
      token: RuleTerminalToken[C],
      nextState: NonTerminalToken,
      rule: Rule[C],
      parentTransition: Option[Transition]
  ) {
    val depth: Int = parentTransition.fold(0)(_.depth + 1)
  }
  private case class Production(
      rule: Rule[C],
      parentTransition: Option[Transition]
  ) {
    val depth: Int = parentTransition.fold(0)(_.depth + 1)
  }

  private case class StateLookup(
      token: NonTerminalToken,
      transitions: Seq[Transition],
      productions: Seq[Production],
      fallback: NonTerminalToken,
      output: Option[NonTerminalToken]
  ) {
    def findTransition(token: C): Option[NonTerminalToken] =
      transitions
        .find { transition =>
          transition.token.matches(token)
        }
        .map(_.nextState)
  }

  private class AhoCorasickAutomaton(
      lookupTable: Map[NonTerminalToken, StateLookup],
      currentStateToken: NonTerminalToken
  ) {
    private val currentState: StateLookup = lookupTable(currentStateToken)

    @tailrec
    private def fallbackUntilHasTransition(
        state: StateLookup,
        token: C
    ): StateLookup = {
      val nextState = state.findTransition(token)

      nextState match {
        case Some(nextStateToken) =>
          lookupTable(nextStateToken)
        case None =>
          if (state.token == state.fallback)
            state
          else
            fallbackUntilHasTransition(lookupTable(state.fallback), token)
      }
    }

    @tailrec
    private def getAllProductions(
        state: StateLookup,
        acc: Seq[Production] = Nil
    ): Seq[Production] = {
      val newAcc = acc ++ state.productions

      state.output match {
        case Some(value) => getAllProductions(lookupTable(value), newAcc)
        case None        => newAcc
      }
    }

    def processToken(token: C): (AhoCorasickAutomaton, Seq[Production]) = {
      val nextState = fallbackUntilHasTransition(currentState, token)
      val productions = getAllProductions(nextState)

      new AhoCorasickAutomaton(lookupTable, nextState.token) -> productions
    }
  }

  private val rootAutomaton: AhoCorasickAutomaton = {
    val temporaryStates = grammar.rules
      .groupBy(_.leftHandSide)
      .map { case (token, rules) =>
        val rulesWithRightHandSide =
          rules.map(rule => rule -> rule.rightHandSide)
        val transitions = rulesWithRightHandSide.collect {
          case (
                rule,
                (a: RuleTerminalToken[C]) :: (b: NonTerminalToken) :: Nil
              ) =>
            Transition(a, b, rule, None)
        }
        val productions = rulesWithRightHandSide.collect { case (rule, Nil) =>
          Production(rule, None)
        }
        token -> StateLookup(token, transitions, productions, token, None)
      }

    val statesWithLinks =
      mutable.Map(
        grammar.startToken -> temporaryStates(grammar.startToken)
      )

    @tailrec
    def findStrictSuffix(
        token: NonTerminalToken,
        nextToken: RuleTerminalToken[C],
        excludeToken: NonTerminalToken
    ): NonTerminalToken = {
      val currentState = statesWithLinks(token)

      val suffixToken = currentState.transitions
        .find { transition =>
          transition.token == nextToken && transition.nextState != excludeToken
        }
        .map(_.nextState)

      suffixToken match {
        case Some(value) => value
        case None =>
          if (currentState.fallback != token)
            findStrictSuffix(currentState.fallback, nextToken, excludeToken)
          else
            token
      }
    }

    @tailrec
    def fillLinks(queue: Seq[NonTerminalToken]): Unit = {
      queue match {
        case token :: tail =>
          val currentState = statesWithLinks(token)

          val addToQueue = currentState.transitions.map {
            case transition @ Transition(matchToken, nextStateToken, _, _) =>
              val nextState = temporaryStates(nextStateToken)
              val strictSuffix =
                findStrictSuffix(token, matchToken, nextStateToken)
              val suffixState = statesWithLinks(strictSuffix)
              val outputLink =
                if (suffixState.productions.nonEmpty)
                  Some(strictSuffix)
                else
                  suffixState.output
              statesWithLinks(nextStateToken) = nextState
                .copy(
                  fallback = strictSuffix,
                  output = outputLink,
                  transitions = nextState.transitions.map(
                    _.copy(parentTransition = Some(transition))
                  ),
                  productions = nextState.productions.map(
                    _.copy(parentTransition = Some(transition))
                  )
                )
              nextStateToken
          }

          fillLinks(tail ++ addToQueue)
        case Nil => ()
      }
    }

    fillLinks(Seq(grammar.startToken))

    new AhoCorasickAutomaton(statesWithLinks.toMap, grammar.startToken)
  }

  override def produceRawParseTrees(tokens: Iterable[C]): Seq[SubTree[C]] = {
    val rawProductions = parseInner(rootAutomaton, tokens)
    val shiftedProductions =
      mutable.ListBuffer.empty[mutable.ListBuffer[Production]]

    rawProductions.zipWithIndex.foreach { case (productions, index) =>
      shiftedProductions += mutable.ListBuffer.empty[Production]
      productions.foreach { production =>
        if (production.depth > 0) {
          val targetIndex = index - production.depth + 1
          val targetIndexDepth =
            shiftedProductions(targetIndex).headOption.fold(0)(_.depth)

          if (targetIndexDepth < production.depth)
            shiftedProductions(targetIndex).clear()

          if (targetIndexDepth <= production.depth)
            shiftedProductions(targetIndex) += production
        }
      }
    }

    val matchNodes = tokens.zipWithIndex.map { case (token, index) =>
      MatchResultNode(token, index)
    }.toSeq

    val productions = shiftedProductions.map(_.toSeq).toSeq

    cartesianProduct(pickLongestMatches(matchNodes, productions, 0, Nil))
      .flatMap {
        case head :: tail =>
          Some(keywordsRule.buildSubTree(head, tail))
        case Nil =>
          None
      }
  }

  @tailrec
  private def pickLongestMatches(
      matchNodes: Seq[MatchResultNode[C]],
      productions: Seq[Seq[Production]],
      currIndex: Int,
      acc: Seq[Seq[SubTree[C]]]
  ): Seq[Seq[SubTree[C]]] = {
    if (currIndex >= matchNodes.size)
      acc
    else {
      val currIndexProductions = productions(currIndex)
      currIndexProductions match {
        case head :: _ =>
          val currIndexTrees =
            currIndexProductions.map(buildSubTree(matchNodes, _, currIndex))
          pickLongestMatches(
            matchNodes,
            productions,
            pickNextIndex(matchNodes, currIndex + head.depth),
            acc :+ currIndexTrees
          )
        case Nil =>
          pickLongestMatches(matchNodes, productions, currIndex + 1, acc)
      }
    }
  }

  @tailrec
  private def pickNextIndex(
      matchNodes: Seq[MatchResultNode[C]],
      index: Int
  ): Int = {
    if (index == 0)
      index
    else {
      val prevTerminal = matchNodes(index - 1).terminal
      if (intersectionTokens.exists(_.matches(prevTerminal)))
        pickNextIndex(matchNodes, index - 1)
      else
        index
    }
  }

  private def buildSubTree(
      matchNodes: Seq[MatchResultNode[C]],
      production: Production,
      currIndex: Int
  ): SubTree[C] = {
    val endIndex = currIndex + production.depth - 1
    val productionTree = production.rule.buildSubTree(
      EmptyNode(currIndex + production.depth - 1),
      Nil
    )
    production.parentTransition match {
      case Some(value) =>
        buildSubTreeTransitions(matchNodes, value, productionTree, endIndex)
      case None => productionTree
    }
  }

  @tailrec
  private def buildSubTreeTransitions(
      matchNodes: Seq[MatchResultNode[C]],
      transition: Transition,
      prevTree: SubTree[C],
      currIndex: Int
  ): SubTree[C] = {
    val newTree =
      transition.rule.buildSubTree(matchNodes(currIndex), Seq(prevTree))

    transition.parentTransition match {
      case Some(value) =>
        buildSubTreeTransitions(matchNodes, value, newTree, currIndex - 1)
      case None => newTree
    }
  }

  @tailrec
  private def parseInner(
      automaton: AhoCorasickAutomaton,
      tokens: Iterable[C],
      acc: Seq[Seq[Production]] = Nil
  ): Seq[Seq[Production]] = {
    tokens.headOption match {
      case Some(value) =>
        val (newState, productions) = automaton.processToken(value)
        parseInner(newState, tokens.tail, acc :+ productions)
      case None =>
        acc
    }
  }
}
