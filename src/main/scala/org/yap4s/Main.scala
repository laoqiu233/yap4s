package org.yap4s

import org.yap4s.core.grammar.modify.{
  ChomskyNormalForm,
  RemoveEmptyProductions,
  RemoveUnitRules
}
import org.yap4s.core.parse.cyk.CYK
import org.yap4s.dsl._

import scala.language.implicitConversions

object Main {

  def main(args: Array[String]): Unit = {
    // Classic "x, y and z" example grammar for a list of names
    val grammar = BuildGrammar[Char] using { grammar =>
      // Define non-terminal tokens
      val Spaces = grammar fragment "spaces"
      val SpacesOrEmpty = grammar fragment "spaces_or_empty"
      val Letter = grammar parses [Char] "letter"
      val Name = grammar parses [String] "name"
      val ListEnd = grammar parses [String] "list_end"
      val ListElems = grammar parses [Seq[String]] "list_elems"
      val NameList = grammar parses [Seq[String]] "name_list"
      val Names = grammar parses [Seq[String]] "names"

      // Define rules
      grammar rule Spaces := ' ' ++ Spaces || ' '
      grammar rule SpacesOrEmpty := Spaces || Empty
      grammar rule Letter := ('a' to 'z').map(!_)

      grammar rule Name := Letter ++ Name map { case (c, str) =>
        c +: str
      }
      grammar rule Name := Letter.map(_.toString)

      grammar rule ListEnd := Spaces ++ 'a' ++ 'n' ++ 'd' ++ Spaces ++ Name

      grammar rule ListElems := SpacesOrEmpty ++ ',' ++ SpacesOrEmpty ++ Name ++ ListElems map {
        case (str, strings) => str +: strings
      }
      grammar rule ListElems := Empty as Nil

      grammar rule NameList := (ListElems ++ ListEnd map {
        case (strings, str) => strings :+ str
      }) || (Empty as Nil)

      grammar rule Names := Name ++ NameList map { case (str, strings) =>
        str +: strings
      }

      (grammar startsWith Names)
        .modifyWith(ChomskyNormalForm)
        .modifyWith(RemoveEmptyProductions)
        .modifyWith(RemoveUnitRules)
    }

    val parser = grammar parseWith CYK

    println(parser.produceResults("billy , test   , awe   and         silly"))
  }
}
