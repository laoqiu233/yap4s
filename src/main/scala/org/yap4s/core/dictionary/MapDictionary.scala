package org.yap4s.core.dictionary

case class MapDictionary[V, C, I <: Iterable[C]](map: Map[V, Seq[I]])
    extends Dictionary[V, C, I] {
  override def getByValue(value: V): Seq[I] = map.getOrElse(value, Nil)

  override def getValues: Seq[V] = map.keys.toSeq

  override def getAll: Seq[(V, I)] = map.toSeq.flatMap { case (k, v) =>
    v.map(k -> _)
  }
}
