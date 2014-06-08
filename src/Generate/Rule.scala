package edu.cmu.lti.nlp.amr.Generate
import edu.cmu.lti.nlp.amr._

import scala.util.matching.Regex
import scala.collection.mutable.{Map, Set, ArrayBuffer}

case class Rule(lhs: String,
                args: Vector[String],
                prefix: String,
                left: List[(String, Int, String)],      // left realization
                lex: String,                            // lexical content
                right: List[(String, Int, String)],     // right realization
                end: String,
                pos: String) {
    def mkRule(verbose: Boolean = true) : String = {
        //"(X "+(lhs ::: args.toList.map(x => (x,"["+x+"]"))).sortBy(_._1).map(_._2).mkString(" ")+") ||| "+rhs(verbose)
        lhs+" ||| "+rhs(verbose)
    }
    def rhs(verbose: Boolean) : String = {
        (prefix+" "+left.map(x => argStr(x), verbose)+" "+lex+" "+right.map(x => argStr(x))+" "+end).replaceAll("^ | $","")
    }
    def argStr(arg: (String, Int, String), verbose: Boolean) : String = {
        if (verbose) {
            (arg._1+" ["+args(arg._2)+"] "+arg._3).replaceAll("^ | $","")
        } else {
            (arg._1+" ["+arg._2.toString+"] "+arg._3).replaceAll("^ | $","")
        }
    }
}

object Rule {
    def mkLhs(x: Node, includeArgs: Boolean = false) : String = {
        val (label, node) = x
        val concept = node.conceptStr
        val children = if (includeArgs) {
            node.children.map(x => (labelStr(x._1), x._2)).sortBy(_._1)
        } else {
            node.children.filter(x => x._2.span == node.span).map(x => (labelStr(x._1), x._2)).sortBy(_._1)
        }
        if (children.size == 0) {
            if (node.concept.startsWith("\"")) {
                "(S "+concept+")"
            } else {
                "(X "+concept+")"
            }
        } else {
            // Example: (X (X hit-01) (ARG0 ___) (ARG1 ___))
            val list = for (x <- children) yield {
                "("+x._1+" "+(if (x._2.span == node.span) { mkLhs(x) } else { "["+x._1+"]" } +")")
            }
            "(X (X "+concept+") "+list.mkString(" ")+")"
        }
    }

    def mkLhsList(node: Node) : List[(String, String)] = {
        ("#", "(X " + node.conceptStr + ")") :: node.children.filter(x => x._2.span == node.span).map(x => label=labelStr(x._1); (label, "("+label+" "+mkLhs(x._2)+")"))
    }
}

