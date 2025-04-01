package org.yap4s.core.grammar.modify

trait GrammarModification

object GrammarModification {
  case object RemoveEmptyProductions extends GrammarModification
  case object RemoveUnitRules extends GrammarModification
  case object ChomskyNormalForm extends GrammarModification
}
