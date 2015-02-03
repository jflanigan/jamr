package edu.cmu.lti.nlp.amr.Generate
import edu.cmu.lti.nlp.amr._

import scala.util.matching.Regex
import scala.collection.mutable.{Map, Set, ArrayBuffer}

case class Arg(left: String, label: String, right: String) {
    val tag : String = left.replaceAllLiterally(" ","_")+"_"+label+"_"+right.replaceAllLiterally(" ","_")
    def ruleStr(index: Int) : String = { left + " [" + index.toString +"] " + right }
    override def toString : String = { left + " [" + label +"] " + right }
}

object Arg {
    val START : Arg = Arg("", "<START>", "")
    val STOP : Arg = Arg("", "<STOP>", "")
    val CONCEPT : Arg = Arg("", "<CONCEPT>", "")
    def Default(label: String) : Arg = Arg("", label, "")   // default pass through no words left or right
}

case class ConceptInfo(realization: PhraseConceptPair, position: Int)


case class Rule(argRealizations: List[Arg],               // Sorted list
                concept: ConceptInfo,
                prefix: String,
                //left: List[(String, Int, String)],      // left realization (Int is index into args vector)
                //lex: String,                          // lexical content
                //right: List[(String, Int, String)],     // right realization (Int is index into args vector)
                end: String) {
    def args : List[String] = { argRealizations.map(x => x.label) }
    def left[A](list: List[A]) : List[A] = { list.take(concept.position) }  // takes the first n
    def right[A](list: List[A]) : List[A] = { list.drop(concept.position) } // drops the first n

    def argsWithIndices : List[(Arg, Int)] = {
        argsArgRealizations.zipWithIndex.sortBy(x => x._1.label).zipWithIndex.sortBy(x => x._1._2).map(x => (x._1._1, x._2))
    }

    def mkRule(withArgLabel: Boolean /*= true*/) : String = {
        //"(X "+(lhs ::: args.toList.map(x => (x,"["+x+"]"))).sortBy(_._1).map(_._2).mkString(" ")+") ||| "+rhs(verbose)
        lhs+" ||| "+rhs(verbose)
    }

    def rhs(withArgLabel: Boolean) : String = {
        def printArg(list: List[(Arg, Int)]) : String = {
            if (withArgLabel) {
                // output is "left [ARG0] right"
                list.map(x => x._1.toString).mkString(" ")
            } else {
                // output is "left [1] right"
                list.map(x => x._1.ruleStr(x._2)).mkString(" ")
            }
        }

        return (prefix+" "+printArg(left(argsWithIndices))+" "+lex+" "+printArg(right(argsWithIndices))+" "+end).replaceAll("^ | $","")
    }

    def lhs : String = {
        return graphToCFG(lhsToGraph)
    }

    def lhsToGraph : Node = {
        val root : Node = Graph.parse(concept.realization.graphFrag)  // TODO: this is slow
        root.children = root.children ::: argRealizations.map(x => Node("", None, "<VARIABLE>", List(), List(), List(), None, new ArrayBuffer()))
        return root
    }

    override def toString() : String = {    // serialize the rule into string format that can loaded using Rule.apply(string) 
        def argF(arg: (String, Int, String)) : String = {
            escape(arg._1, '_') + "_" + arg._2.toString + "_" + escape(arg._3, '_'))
        }
        return List(args.mkString(" "), prefix, left.map(x => escape(argF(x),',')).mkString(","), concept.toString, right.map(x => escape(argF(x),',')).mkString(","), end).mkString(" ||| ")
    }

    // below is old code
    def argStr(arg: (String, Int, String), verbose: Boolean) : String = {
        if (verbose) {
            (arg._1+" ["+args(arg._2)+"] "+arg._3).replaceAll("^ | $","")
        } else {
            (arg._1+" ["+arg._2.toString+"] "+arg._3).replaceAll("^ | $","")
        }
    }

    def lhs : String = {
        // TODO: cache this
        val frag : Node = Graph.parse(concept.realization.graphFrag)  // TODO: this is slow
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

}

object Rule {
    def apply(string: String) : Rule = {    // inverse of Rule.toString
        // TODO: update this to the new code
        val ruleRegex = """([^|]*) \|\|\| ([^|]*) \|\|\| ([^|]*) \|\|\| (.*) \|\|\| ([^\|]*) \|\|\| ([^|]*)""".r
        val argRegex = """\(.*)\t[0-9]+\t\(.*\)""".r
        val ruleRegex(argsStr, prefix, leftStr, concept, rightStr, end) = string
        val args = argsStr.split(" ").toVector
        val left : List[(String, Int, String)] = unEscapeArray(leftStr,',').map(x => { val argRegex(l,i,r) = unEscape(x,'_'); (l, i.toInt, r) }).toList
        val right : List[(String, Int, String)] = unEscapeArray(rightStr,',').map(x => { val argRegex(l,i,r) = unEscape(x,'_'); (l, i.toInt, r) }).toList
        return Rule(args, prefix, left, PhraseConceptPair(concept), right, end)
    }

    def extract(node: Node,                 // extract a rule from a node in a graph, if it can be extracted (no overlapping children)
                graph: Graph,
                sentence: Array[String],
                pos: Array[String],
                spans: Map[String, (Int, Int)]) : Option[Rule] = {
        case class Child(label: String, node: Node, start: Int, end: Int)

        val (ruleStart, ruleEnd) = spans(node.id)
        val children : List[Child] = (
            for { (label, child) <- children
                  if spans.contains(child.id)           // filter to aligned children
                  if child.span != node.span            // that are in a different fragment
                } yield {
                    val (start, end) = spans(child.id)
                    Child(Label(label), child, start.get, end.get)
                } ).sortBy(x => x.end)

        //logger(1, "children = "+children.toString)

        if (children.size > 0 && !(0 until children.size-1).exists(i => children(i).start > children(i+1).end)) { // if child spans overlap then no rule can be extracted
            var outsideLower = ruleStart.get
            //do { outsideLower -= 1 } while (outsideLower >= 0 && !spanArray(outsideLower))
            //outsideLower += 1
            var outsideUpper = ruleEnd.get
            //while (outsideUpper < sent.size && !spanArray(outsideUpper)) {
            //    outsideUpper += 1
            //}

            val args : List[Children] = children.sortBy(x => x.label)
            val span = graph.spans(node.spans(0))
            val lowerChildren : Vector[(Children, Int)] = args.zipWithIndex.filter(x => x._1.start < span.start).sortBy(_._1.start).toVector
            val upperChildren : Vector[(Children, Int)] = args.zipWithIndex.filter(x => x._1.start > span.end).sortBy(_._1.start).toVector
            val prefix : String = sentence.slice(outsideLower, ruleStart.get)
            val end : String = sentence.slice(myEnd.get, outsideUpper)
            val pos : Array[String] = pos.slice(span.start, span.end)

 /********************** TODO ******************
 *
 * Have alignments for edges and use them here
 *
 ***********************************************/

            val argsList = args.map(x => x.label).toVector
            var left = (0 until lowerChildren.size-1).map(
                i => ("", x._2, sentence.slice(lowerChildren(i)._2.end, lowerChildren(i+1)._2.start))).toList
            left = left ::: List("", lowerChilren.last._2, sentence.slice(lowerChilren.last._1.end, span.start))
            var right = (1 until upperChildren.size).map(
                i => (sentence.slice(upperChildren(i-1)._2.end, upperChildren(i)._2.start)), x._2, "").toList
            right = (sentence.slice(span.end, upperChilren.head._1.end), upperChilren.last._2, "") :: right
            //val lhs = Rule.mkLhs(node, includeArgs=true)

            val rule = Rule(argsList, prefix, left, PhraseConceptPair.fromSpan(span, pos), right, end)

            Some(rule)
        } else {
            None
        }
    }

    abstractRule(rule: Rule) : Rule = {     // turn a lexicalized rule into an abstract (unlexicalized) rule
        val concept = PhraseConceptPair("###", rule.concept.graphFrag, rule.concept.fullPos, rule.concept.headPos)
        return Rule(rule.args, rule.prefix, rule.left, concept, right, end)
    }

    def conceptStr(node: Node) : String = {
        return node.concept.replaceAll("""\(""", "-LBR-").replaceAll("""\)""", "-RBR-")
    }

    def graphToCFG(x: Node) : String = {
        val concept = conceptStr(node)
        val children = node.children.map(x => (labelStr(x._1), x._2)).sortBy(_._1)
        if (children.size == 0) {
            if (node.concept.startsWith("\"")) {
                "(S "+concept+")"
            } if (node.concept == "<VARIABLE>") {
                "[X]"
            } else {
                "(X "+concept+")"
            }
        } else {
            // Example: (X (X hit-01) (ARG0 ___) (ARG1 ___))
            assert(!node.concept == "<VARIABLE>", "Error: cannot have a variable with children")
            val list : List[String] = for (x <- children) yield {
                "("+x._1+" "+graphToCFG(x._2)")"
            }
            "(X (X "+concept+") "+list.sorted.mkString(" ")+")"
        }
    }

    // below is old code

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
