package org.yap4s

import org.yap4s.core.dictionary.MapDictionary
import org.yap4s.core.grammar.Grammar
import org.yap4s.core.parse.lex.LexingKeywords
import org.yap4s.dsl.CharIsTerminalToken

import scala.language.implicitConversions

object Main {

  def main(args: Array[String]): Unit = {
    trait Mark
    case object A extends Mark
    case object B extends Mark
    val dict = MapDictionary[Mark, Char, Iterable[Char]](
      Map(
        A -> Seq(" ab ", " ba "),
        B -> Seq(" ab ")
      )
    )

    val (startToken, rules) = dict.toRegularRules
    val grammar = Grammar[Seq[Mark], Char](rules, startToken, Nil)

    val parser =
      grammar parseWith LexingKeywords(Seq(CharIsTerminalToken.wrap(' ')))

    val results = parser.produceResults(" ab ba ")

    println(results)
  }
}
