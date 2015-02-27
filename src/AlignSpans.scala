package edu.cmu.lti.nlp.amr

import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer

/****************************** Align Spans *****************************/
object AlignSpans {

    def logUnalignedConcepts(node: Node) {
        if (node.spans.size == 0) {
            logger(1, "WARNING: Unaligned concept "+node.concept)
        }
        for ((_, child) <- node.topologicalOrdering) {
            logUnalignedConcepts(child)
        }
    }

    def alignSpans(sentence: Array[String], /*stemmedSentence: Array[List[String]],*/ graph: Graph, wordAlignments: Array[Option[Node]]) : Array[Option[Int]] = {
        val spanAlignments = new Array[Option[Int]](sentence.size)
        for (i <- Range(0, sentence.size)) {
            spanAlignments(i) = None
        }
        createSpans(sentence, /*stemmedSentence,*/ graph.root, wordAlignments, spanAlignments, None, graph.spans)
        logger(3, graph.spans.toString)
        return spanAlignments
    }

    val specialRelations1 : List[String] = List(":ARG.*-of")
    val specialRelations2 : List[String] = List(":unit")
    val specialConcepts : Set[String] = Set(
        "name","country","person","date-entity","organization","city","thing","company","monetary-quantity","continent","mass-quantity","religious-group","political-party","distance-quantity","criminal-organization","research-institute","date-interval","temporal-quantity","world-region","ethnic-group","university")
// "govern-01"

    def createSpans(sentence: Array[String], /*stemmedSentence: Array[List[String]],*/ node: Node, wordAlignments: Array[Option[Node]], spanAlignments: Array[Option[Int]], spanIndex: Option[Int], spans: ArrayBuffer[Span]) : Option[Span] = {
    // Returns the span for 'node'
//Span(var start: Int, var end: Int, var nodeIds: List[String], var words: String, var amr: Node
//Node(var id: String, name: Option[String], concept: String, var relations: List[(String, Node)], var topologicalOrdering: List[(String, Node)], var variableRelations: List[(String, Node)], var alignment: Option[Int], var span: Option[Int])
        var mySpan = Span(sentence.size, 0, List(node.id), "", Node("", node.name, node.concept, List[(String, Node)](), List[(String, Node)](), List[(String, Node)](), None, ArrayBuffer()), false) // will update later
        var valid = false
        if (specialConcepts contains node.concept) {
            var mySpanIndex = spanIndex
            if (spanIndex == None) {
                mySpanIndex = Some(spans.size)
                spans.append(mySpan) // so we can pass a valid spanIndex
            }
            for ((relation, child) <- node.topologicalOrdering) {
                val span = createSpans(sentence, /*stemmedSentence,*/ child, wordAlignments, spanAlignments, mySpanIndex, spans)
                if (span.size != 0) {
                    val Some(Span(start,end,nodeIds,_,amr,_)) = span // TODO: is this code right?
                    mySpan.start = min(mySpan.start, start)
                    mySpan.end = max(mySpan.end, end)
                    mySpan.nodeIds = mySpan.nodeIds ::: nodeIds
                    mySpan.amr.topologicalOrdering = (relation, amr) :: mySpan.amr.topologicalOrdering
                    mySpan.amr.relations = (relation, amr) :: mySpan.amr.relations
                }
            }
            mySpan.amr.topologicalOrdering = mySpan.amr.topologicalOrdering.reverse
            mySpan.amr.relations = mySpan.amr.relations.reverse
            // TODO: check that the span is valid and update spanAlignments
            valid = true
            for (i <- Range(mySpan.start, mySpan.end)) {
                if (spanAlignments(i) != None) {
                    if (spanAlignments(i) != mySpanIndex) {
                        valid = false   // there's a word in the span aligned to a different span, so this is not a valid span
                    }
                }
            }
            mySpan.words = sentence.slice(mySpan.start, mySpan.end).mkString(" ")
            if (spanIndex == None) {    // we need to save the span
                val Some(index) = mySpanIndex
                spans(index) = mySpan
            }
            if (mySpanIndex != None) {  // replaces node.spans = mySpanIndex
                val Some(myspanindex) = mySpanIndex
                if (node.spans.size == 0) {
                    node.spans += myspanindex
                } else {
                    node.spans(0) = myspanindex
                }
            }
        } else {
            if (node.alignment != None) {
                val Some(alignment) = node.alignment
                mySpan.start = alignment
                mySpan.end = alignment + 1
                mySpan.words = sentence(alignment)
                if (spanIndex == None) {  // we need to insert the span ourselves
                    spanAlignments(alignment) = Some(spans.size)
                    spans.append(mySpan)
                } else {
                    spanAlignments(alignment) = spanIndex  // index to this span
                }
                if (node.spans.size == 0) {
                    node.spans += spans.size
                } else {
                    node.spans(0) = spans.size  // TODO: check to see if there are other spans already?
                }
                valid = true
            }
            for ((relation, child) <- node.topologicalOrdering) {
                createSpans(sentence, /*stemmedSentence,*/ child, wordAlignments, spanAlignments, None, spans)
            }
        }
        return if(valid) { Some(mySpan) } else { None}
    }

}

