package org.yap4s.core.parse

import org.yap4s.core.grammar.modify.GrammarModification
import org.yap4s.core.model.MatchResult
import org.yap4s.core.model.MatchResult.{ModifiedSubTree, SubTree}

trait ReverseModificationsParser[-C] extends Parser[C] {
  protected def appliedModifications: Seq[GrammarModification]

  abstract override def produceRawParseTrees(
      tokens: Iterable[C]
  ): Seq[SubTree] = {
    val producedTrees = super.produceRawParseTrees(tokens)
    producedTrees.map { subTree =>
      appliedModifications.reverse.foldLeft(subTree)(reverseModificationSubTree)
    }
  }

  private def reverseModificationSubTree(
      subTree: SubTree,
      modificationToReverse: GrammarModification
  ): SubTree = {
    val newTree = subTree match {
      case tree: ModifiedSubTree
          if tree.appliedModification.modification == modificationToReverse =>
        val (reversedHead, reversedTail) = tree.appliedModification
          .reverseSubTreeModification(tree.headNode, tree.tailNodes)
        val newTree = tree.appliedModification.parentSubTreeBuilder
          .buildSubTree(reversedHead, reversedTail)
        reverseModificationSubTree(newTree, modificationToReverse)
      case other => other
    }

    newTree.rebuildWithChildren(
      reverseModifications(newTree.headNode, modificationToReverse),
      newTree.tailNodes.map(reverseModifications(_, modificationToReverse))
    )
  }

  private def reverseModifications(
      node: MatchResult,
      modificationToReverse: GrammarModification
  ): MatchResult =
    node match {
      case tree: SubTree =>
        reverseModificationSubTree(tree, modificationToReverse)
      case other => other
    }
}
