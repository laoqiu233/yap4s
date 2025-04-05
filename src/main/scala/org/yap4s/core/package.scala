package org.yap4s

import scala.annotation.tailrec

package object core {
  def cartesianProduct[T](choices: Seq[Seq[T]]): Seq[Seq[T]] = {
    cartesianProductInner(choices)
  }

  @tailrec
  private def cartesianProductInner[T](
      choices: Seq[Seq[T]],
      acc: Seq[Seq[T]] = Seq(Nil)
  ): Seq[Seq[T]] =
    choices match {
      case Nil =>
        acc
      case head :: tail =>
        val newAcc = acc.flatMap(seq => head.map(head => seq :+ head))
        cartesianProductInner(tail, newAcc)
    }
}
