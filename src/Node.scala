package edu.cmu.lti.nlp.amr

import scala.util.matching.Regex
import scala.collection.mutable.{Map, Set, ArrayBuffer}

//case class Var(node: Node, name: String)    // TODO: Remove this? Var is redundant, because the name can be found using node.name (but node.name sometimes can be None, which it should not be)

case class Node(var id: String, var name: Option[String], concept: String, var relations: List[(String, Node)], var topologicalOrdering: List[(String, Node)], var variableRelations: List[(String, Node)], var alignment: Option[Int], var spans: ArrayBuffer[Int] /* TODO: change to something immutable (ie List) Interacts if a span gets copied from this span */) {

    def children: List[(String, Node)] = topologicalOrdering    // property (see Ch 18.2 stairway book)
    def children_= (c: List[(String, Node)]) {
        topologicalOrdering = c
    }

    def isAligned(graph: Graph) : Boolean = {
        // Returns true if this node has an non-coref span alignment
        return (spans.map(x => !graph.spans(x).coRef) :\ false)(_ || _)
    }
    
    def isConstant : Boolean = {
        return concept.startsWith("\"") || concept.matches("[0-9].*") || concept.matches("-")
    }

    def span : Option[Int] = {  // returns the primary span, if exists (span(0) is always the primary)
        return if (spans.size > 0) { Some(spans(0)) } else { None }
    }

    def addSpan(span: Int, coRef: Boolean) {
        if (coRef) {
            spans += span
        } else {
            if (spans.size > 0) {
                logger(0,"WARNING ADDING ANOTHER SPAN TO NODE "+concept)
                logger(0,spans.toString+" + "+span.toString)
            }
            spans.+=:(span) // prepend
        }
    }

    def nameStr : String = {
        name match {
            case None => ""
            case Some(n) => n 
        }
    }

    override def toString() : String = {
        prettyString(0, false, Set.empty[String], sorted = true)    // need to be sorted so there is a canonical representation of graph fragments (used in concept ID eval, etc)
    }

    def prettyString(detail: Int, pretty: Boolean, vars: Set[String], sorted: Boolean = false, indent: String = "") : String = {
    // detail = 0: Least detail. No variables names or node ids.
    //             (date-entity :day 5 :month 1 :year 2002)

    // detail = 1: Variable names included.
    //             (d / date-entity :day 5 :month 1 :year 2002)

    // detail = 2: Nodes are labelled with id.
    //             ([0] d / date-entity :day [0.2] 5 :month [0.1] 1 :year [0.0] 2002)

    // Boolean 'pretty' indicates whether to indent into pretty format or leave on one line

        def sort(list: List[(String, Node)]) = if (sorted) { list.sortBy(x => x._1+" "+x._2.concept) } else { list }

        var nextIndent = ""
        var prefix = "" 
        if (pretty) {   // prefix for the children (so that it goes ':ARG0 concept' on the same line)
            nextIndent = indent + "      "  // indent by six
            prefix = "\n" + nextIndent
        }
        if (name != None) {
            val Some(n) = name
            if ((topologicalOrdering.size +variableRelations.size) != 0) {      // Concept with name and children
                detail match {
                    case 0 =>
                "("+concept+" "+(sort(topologicalOrdering).map(x => prefix+x._1+" "+x._2.prettyString(detail, pretty, vars, sorted, nextIndent)) :::
                                 sort(variableRelations).map(x => prefix+x._1+" "+x._2.concept)).mkString(" ")+")"
                    case 1 =>
                "("+n+" / "+concept+" "+(sort(topologicalOrdering).map(x => prefix+x._1+" "+x._2.prettyString(detail, pretty, vars, sorted, nextIndent)) :::
                                 sort(variableRelations).map(x => prefix+x._1+" "+x._2.name.get)).mkString(" ")+")"
                    case 2 =>
                "(["+id+"] "+n+" / "+concept+" "+(sort(topologicalOrdering).map(x => prefix+x._1+" "+x._2.prettyString(detail, pretty, vars, sorted, nextIndent)) :::
                                 sort(variableRelations).map(x => prefix+x._1+" ["+x._2.id+"] "+x._2.name.get)).mkString(" ")+")"
                }
            } else {                        // Concept with name, but no children
                detail match {
                    case 0 =>
                        concept
                    case 1 => {
                        if (isConstant && !vars.contains(name.get)) {       // WARNING: If you change this filter for concepts that get variable names, be sure also change the code that selects the root in Alg1, Alg2, and Greedy decoders (or any decoder that uses isConstant) (change the filter)
                            concept
                        } else {
                            "("+n+" / "+concept+")"
                        }
                    }
                    case 2 =>
                        "(["+id+"] "+n+" / "+concept+")"
                }
            }
        } else if ((topologicalOrdering.size + variableRelations.size) == 0) {   // Concept with no name and no children
            if (detail < 2) {
                concept
            } else {
                "["+id+"] "+concept
            }
        } else {                            // Concept with no name but has children
            if (detail == 0) {
                "("+concept+" "+(sort(topologicalOrdering).map(x => prefix+x._1+" "+x._2.prettyString(detail, pretty, vars, sorted, nextIndent)) :::
                                 sort(variableRelations).map(x => prefix+x._1+" "+x._2.concept)).mkString(" ")+")"
            } else if (detail == 1) {
                "("+concept+" "+(sort(topologicalOrdering).map(x => prefix+x._1+" "+x._2.prettyString(detail, pretty, vars, sorted, nextIndent)) :::
                                 sort(variableRelations).map(x => prefix+x._1+" "+x._2.name)).mkString(" ")+")"
            } else {
                "(["+id+"] "+concept+" "+(sort(topologicalOrdering).map(x => prefix+x._1+" "+x._2.prettyString(detail, pretty, vars, sorted, nextIndent)) :::
                            sort(variableRelations).map(x => prefix+x._1+" ["+x._2.id+"] "+x._2.name)).mkString(" ")+")"
            }
        }
    }
}

