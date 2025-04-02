package org.yap4s.core.dictionary

trait Dictionary[V, C, I <: Iterable[C]] {
  def getByValue(value: V): Seq[I]
  def getValues: Seq[V]
  def getAll: Seq[(V, I)]
}
