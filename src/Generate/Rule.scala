package edu.cmu.lti.nlp.amr.Generate
import edu.cmu.lti.nlp.amr._

import scala.util.matching.Regex
import scala.collection.mutable.{Map, Set, ArrayBuffer}

case class Rule(args: Vector[String],
                prefix: String,
                left: List[(String, Int, String)],      // left realization (Int is index into args vector)
                //lex: String,                          // lexical content
                concept: PhraseConceptPair,             // PhraseConceptPair
                right: List[(String, Int, String)],     // right realization (Int is index into args vector)
                end: String) {
    def lhs : String = {                        // TODO: cache this
        val frag : Node = Graph.parse(concept)  // TODO: this is slow
        if (frag.children.size + args.size == 0) {   // no children
            if (frag.concept.startsWith("\"")) {
                "(S "+frag.concept+")"
            } else {
                "(X "+frag.concept+")"
            }
        } else {
            // Example: (X (X hit-01) (ARG0 ___) (ARG1 ___))
            val list = for (x <- frag.children) yield {
                "("+x._1+" "+(if (x._2.span == node.span || !sameSpan) { mkLhs(x._2) } else { "["+x._1+"]" } +")")
            }
            "(X (X "+frag.concept+") "+list.mkString(" ")+")"
        }
    }
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
    override def toString() : String = {
        def argF(arg: (String, Int, String)) : String = {
            escape(arg._1, '_') + "_" + arg._2.toString + "_" + escape(arg._3, '_'))
        }
        return List(args.mkString(" "), prefix, left.map(x => escape(argF(x),',')).mkString(","), concept.toString, right.map(x => escape(argF(x),',')).mkString(","), end).mkString(" ||| ")
    }
}

object Rule {
    def apply(string: String) : Rule = {
        val ruleRegex = """([^|]*) \|\|\| ([^|]*) \|\|\| ([^|]*) \|\|\| (.*) \|\|\| ([^\|]*) \|\|\| ([^|]*)""".r
        val argRegex = """\(.*)\t[0-9]+\t\(.*\)""".r
        val ruleRegex(argsStr, prefix, leftStr, concept, rightStr, end) = string
        val args = argsStr.split(" ").toVector
        val left : List[(String, Int, String)] = unEscapeArray(leftStr,',').map(x => { val argRegex(l,i,r) = unEscape(x,'_'); (l, i.toInt, r) }).toList
        val right : List[(String, Int, String)] = unEscapeArray(rightStr,',').map(x => { val argRegex(l,i,r) = unEscape(x,'_'); (l, i.toInt, r) }).toList
        return Rule(args, prefix, left, PhraseConceptPair(concept), right, end)
    }

    def mkLhs(x: Node, includeArgs: Boolean = false, sameSpan: Boolean = true) : String = {
        // sameSpan indicates we are in the same fragment as our parent (so don't process children that are not in our span)
        val concept = node.conceptStr
        val children = if (includeArgs || !sameSpan) {
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
            val list : List[String] = for (x <- children) yield {
                "("+x._1+" "+(if (x._2.span == node.span || !sameSpan) { mkLhs(x._2) } else { "["+x._1+"]" } +")")
            }
            "(X (X "+concept+") "+list.sorted.mkString(" ")+")"
        }
    }

    def mkLhsList(node: Node, sameSpan: Boolean = true) : List[(String, String)] = {
        val children = if (sameSpan) {
            node.children.filter(x => x._2.span == node.span)
        } else {
            node.children
        }
        return ("#", "(X " + node.conceptStr + ")") :: children.map(x => { label=labelStr(x._1); (label, "("+label+" "+mkLhs(x._2, includeArgs=false, sameSpan=sameSpan)+")") } )
    }
}
