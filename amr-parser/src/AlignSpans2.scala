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
import java.util.regex.Pattern
import scala.io.Source
import scala.util.matching.Regex
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer
import scala.util.parsing.combinator._

/****************************** Align Words *****************************/
object AlignSpans2 {

    def align(sentence: Array[String], graph: Graph) {
        val lcSentence = sentence.map(_.toLowerCase).mkString("\t")
        //graph.addAllSpans(sentence, dateEntity)
        graph.addAllSpans(namedEntity(sentence, lcSentence, graph)_)
        //dateEntities(sentence, graph)
        //namedEntities(sentence, graph)
        //specialConcepts(sentence, graph) // un, in, etc
        //singleConcepts(sentence, graph)
        //learnedConcepts(sentence, graph)
    }

    def namedEntity(sentence: Array[String], lcSentence: String, graph: Graph)(node: Node) : List[Span] = {
        logger(3, "Processing concept: " + node.concept)
        logger(3, "lcSentence = " + lcSentence)
        val entityRegex = """(name)""".r
        return node match {
            case Node(_,_,entityRegex(entity),_,children,_,_,_) => 
                if (children.exists(_._1 == ":op1")) {
                    logger(3, "Has an :op1 child")
                    var spanList = List[Span]()
                    val childNodes = for {(relation, node) <- children 
                                     if relation.matches(":op.*")
                                } yield node //(relation, node) ).sortWith((x,y) => x._1 < y._1).map(x => x.2)
                    logger(3, "childNodes = " + childNodes.map(_.concept).toList.toString)
                    val regex = childNodes.map(x => Pattern.quote(getConcept(x.concept).toLowerCase)).mkString("[^a-zA-Z]*").r
                    logger(3, "regex = " + childNodes.map(x => Pattern.quote(getConcept(x.concept).toLowerCase)).mkString("[^a-zA-Z]*"))
                    var matchList = regex.findAllMatchIn(lcSentence).toList
                    logger(3, "matchList = " + matchList)
                    logger(3, "Returning "+matchList.zipWithIndex.map(x => matchToSpan(x._1, node, childNodes, sentence, lcSentence, graph, x._2 > 0)).toString)
                    matchList.zipWithIndex.map(x => matchToSpan(x._1, node, childNodes, sentence, lcSentence, graph, x._2 > 0))
                } else {
                    List()  // TODO: should still return something (first :mod child?)
                }
            case _ => List()
        }
    }

    private def matchToSpan(m: Regex.Match, node: Node, childNodes: List[Node], sentence: Array[String], tabSentence: String, graph: Graph, coRef: Boolean) : Span = {
        // m is a match object which is a match in tabSentence (tabSentence = sentence.mkString('\t'))
        // note: tabSentence does not have to be sentence.mkString('\t') (it could be lowercased, for example)
        val start = getIndex(tabSentence, m.start)
        val end = getIndex(tabSentence, m.end)
        val nodeIds : List[String] = node.id :: childNodes.map(_.id)
        val amr = SpanLoader.getAmr(nodeIds, graph)
        val words = sentence.slice(start, end).mkString(" ")
        val span = if (!coRef) {
                Span(start, end, nodeIds, words, amr, coRef)
            } else {
                Span(start, end, List(node.id), words, SpanLoader.getAmr(List(node.id), graph), coRef)
            }
        return span
    }

    private def getIndex(tabSentence: String, charIndex : Int) : Int = {
        return tabSentence.view.slice(0,charIndex).count(_ == '\t') // views (p.552 stairway book)
    }

    private val ConceptExtractor = """^"?(.+?)-?[0-9]*"?$""".r // works except for numbers

    private def getConcept(conceptStr: String) : String = {
        var ConceptExtractor(concept) = conceptStr
        if (conceptStr.matches("""^[0-9.]*$""")) {
            concept = conceptStr
        }
        return concept
    }

    def alignWords(sentence: Array[String], graph: Graph) : Array[Option[Node]] = {
        val size = sentence.size
        val wordAlignments = new Array[Option[Node]](size)
        val stemmedSentence = new Array[List[String]](size)
        for (i <- Range(0, size)) {
            stemmedSentence(i) = stemmer(sentence(i))
            wordAlignments(i) = None
        }
        logger(2, "Stemmed sentence "+stemmedSentence.toList.toString)
        alignWords(stemmedSentence, graph.root, wordAlignments)
        fuzzyAligner(stemmedSentence, graph.root, wordAlignments)
        return wordAlignments  // Todo: Return spanAlignments
    }

    //private val conceptRegex = """-[0-9]+$""".r
    //private val ConceptExtractor = "([a-zA-Z0-9.-]+ *)|\"([^\"]+)\" *".r
    //private val ConceptExtractor = """([a-zA-Z0-9.-]+)\|(?:"([^ ]+)")""".r
    //private val ConceptExtractor = """"?([a-zA-Z0-9.-]+)"?""".r
    //private val ConceptExtractor = """^"?(.+?)(?:-[0-9]+)"?$""".r
    //private val ConceptExtractor = """^"?(.+?)-?[0-9]*"?$""".r // works except for numbers
    def alignWords(stemmedSentence: Array[List[String]], node: Node, alignments: Array[Option[Node]]) {
        logger(3,"alignWords: node.concept = "+node.concept)
        var ConceptExtractor(concept) = node.concept
        if (node.concept.matches("""^[0-9.]*$""")) {
            concept = node.concept
        }
        var found = false
        for (i <- Range(0, stemmedSentence.size)) {
            for (word <- stemmedSentence(i)) {
                if (word == concept && alignments(i) == None) {
                    if (found) {
                        logger(1, "WARNING: Found duplicate match for concept "+node.concept)
                    } else {
                        logger(3,"concept: "+node.concept+" word: "+word)
                        alignments(i) = Some(node) // point to the current node
                        node.alignment = Some(i)
                    }
                    found = true
                }
            }
        }
        if (!found) {
            //logger(2,"CONCEPT NOT FOUND: "+node.concept+" by searching "+concept)
        }
        for ((_, child) <- node.topologicalOrdering) {
            alignWords(stemmedSentence, child, alignments)
        }
    }

    def fuzzyAligner(stemmedSentence: Array[List[String]], node: Node, alignments: Array[Option[Node]]) {
        var ConceptExtractor(concept) = node.concept
        if (node.concept.matches("""^[0-9.]*$""")) {
            concept = node.concept
        }
        val size = stemmedSentence.size
        var found = false
        val matchlength = new Array[Int](size)
        for (i <- Range(0, size)) {
            matchlength(i) = 0
            for (word <- stemmedSentence(i)) {
                val len = matchLength(word, concept)
                if (len > matchlength(i)) {
                    matchlength(i) = len
                }
            }
        }
        val max = matchlength.max
        if (max >= 4) {
            for (i <- Range(0, size) if (matchlength(i) == max && alignments(i) == None && node.alignment == None)) {
                if (!found) {
                    logger(2,"Fuzzy Matcher concept: "+node.concept+" word: "+stemmedSentence(i)(0))
                    alignments(i) = Some(node)
                    node.alignment = Some(i)
                } else {
                    logger(1, "WARNING: duplicate fuzzy matches for concept "+node.concept)
                }
            }
        }
        if (!found) {
            //logger(4,"CONCEPT NOT FOUND: "+node.concept+" by fuzzy matching "+concept)
        }
        for ((_, child) <- node.topologicalOrdering) {
            fuzzyAligner(stemmedSentence, child, alignments)
        }
    }

    def matchLength(string1: String, string2: String) : Int = {
        var length = 0
        for (i <- Range(0, min(string1.size, string2.size))) {
            if (string1(i) == string2(i) && length == i) {
                length = i + 1
            }
        }
        return length
    }

    def stemmer(word: String) : List[String] = {
        var stems = Wordnet.stemmer(word)
        var numbers = word.toLowerCase match {
            case "one" => List("1")
            case "two" => List("2")
            case "three" => List("3")
            case "four" => List("4")
            case "five" => List("5")
            case "six" => List("6")
            case "seven" => List("7")
            case "eight" => List("8")
            case "nine" => List("9")
            case _ => List()
        }
        var months = word match {
            case "January" => List("1")
            case "February" => List("2")
            case "March" => List("3")
            case "April" => List("4")
            case "May" => List("5")
            case "June" => List("6")
            case "July" => List("7")
            case "August" => List("8")
            case "September" => List("9")
            case "October" => List("10")
            case "November" => List("11")
            case "December" => List("12")
            case _ => List()
        }
        var exceptions = word.toLowerCase match {
            case ";" => List("and")
            case "also" => List("include")
            case "anti" => List("oppose","counter")
            case "but" => List("contrast")
            case "because" => List("cause")
            case "if" => List("cause")
            case "no" => List("-")
            case "not" => List("-")
            case "of" => List("include")
            case "speech" => List("speak")
            case "statement" => List("state")
            case _ => List()
        }
        if (word.matches("""(in|un).*""")) {
            exceptions = word.drop(2) :: exceptions  // should include "-"
        }
        if (word.matches(""".*er""")) {
            exceptions = word.dropRight(2) :: exceptions  // should include "-"
        }
        if (word.matches(""".*ers""")) {
            exceptions = word.dropRight(3) :: exceptions  // should include "-"
        }
        //if (word.matches("""^[0-9]*$""")) {
        //    numbers = word.toInt.toString :: numbers
        //}
        return (word :: word.toLowerCase :: numbers ::: months ::: exceptions ::: stems).distinct
    }

    def logUnalignedConcepts(node: Node) {
        if (node.alignment == None) {
            logger(1, "WARNING: Unaligned concept "+node.concept)
        }
        for ((_, child) <- node.topologicalOrdering) {
            logUnalignedConcepts(child)
        }
    }

}

