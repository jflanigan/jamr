package edu.cmu.lti.nlp.amr

import java.io.File
import java.io.FileOutputStream
import java.io.PrintStream
import java.io.BufferedOutputStream
import java.io.OutputStreamWriter
import java.lang.Math.abs
import java.lang.Math.log
import java.lang.Math.exp
import java.lang.Math.random
import java.lang.Math.floor
import java.lang.Math.min
import java.lang.Math.max
import scala.io.Source
import scala.util.matching.Regex
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer
import scala.util.parsing.combinator._

/****************************** Align Spans *****************************/
object AlignSpans {

    def logUnalignedConcepts(node: Node) {
        //if (node.alignment == None) {
        if (node.span == None) {
            logger(1, "WARNING: Unaligned concept "+node.concept)
        }
        for ((_, child) <- node.relations) {
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

    val specialConcepts : Set[String] = Set("name","country","person","date-entity","organization","city","thing","company","monetary-quantity","continent","mass-quantity","religious-group","political-party","distance-quantity","criminal-organization","research-institute","date-interval","temporal-quantity","world-region","ethnic-group","university")
// "govern-01"

    def createSpans(sentence: Array[String], /*stemmedSentence: Array[List[String]],*/ node: Node, wordAlignments: Array[Option[Node]], spanAlignments: Array[Option[Int]], spanIndex: Option[Int], spans: ArrayBuffer[Span]) : Option[Span] = {
//Span(start: Int, end: Int, words: String, amr: Node)
//Node(name: Option[String], concept: String, relations: List[(String, Node)], var alignment: Option[Int], var span: Option[Int])
        var mySpan = Span(sentence.size, 0, "", Node(node.name, node.concept, List[(String, Node)](), None, None)) // will update later
        var valid = false
        if (specialConcepts contains node.concept) {
            var mySpanIndex = spanIndex
            if (spanIndex == None) {
                mySpanIndex = Some(spans.size)
                spans.append(mySpan) // so we can pass a valid spanIndex
            }
            for ((relation, child) <- node.relations) {
                val span = createSpans(sentence, /*stemmedSentence,*/ child, wordAlignments, spanAlignments, mySpanIndex, spans)
                if (span != None) {
                    val Some(Span(start,end,_,amr)) = span
                    mySpan.start = min(mySpan.start, start)
                    mySpan.end = max(mySpan.end, end)
                    mySpan.amr.relations = (relation, amr) :: mySpan.amr.relations
                }
            }
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
            if (spanIndex == None) {  // we need to save the span
                val Some(index) = mySpanIndex
                spans(index) = mySpan
            }
            node.span = mySpanIndex
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
                node.span = Some(spans.size)
                valid = true
            }
            for ((relation, child) <- node.relations) {
                createSpans(sentence, /*stemmedSentence,*/ child, wordAlignments, spanAlignments, None, spans)
            }
        }
        return if(valid) { Some(mySpan) } else { None}
    }

}

