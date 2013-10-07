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
        val stemmedSentence = sentence.map(stemmer(_))
        val wordToSpan : Array[Option[Int]] = sentence.map(x => None)
        logger(3, "Stemmed sentence "+stemmedSentence.toList.toString)

        val namedEntity = new SpanAligner(sentence, graph) {
            concept = "name"
            tabSentence = sentence.mkString("\t").toLowerCase/*.replaceAll("[^a-zA-Z0-9\t]","")*/
            nodes = node => {
                if (node.children.exists(_._1.matches(":op.*"))) {
                    ("", node) :: node.children.filter(_._1.matches(":op.*"))
                } else {
                    List()
                }
            }
            //words = nodes => { nodes.tail.map(x => Pattern.quote(getConcept(x._2.concept).toLowerCase.replaceAll("[^a-zA-Z0-9\t]",""))).mkString("[^a-zA-Z]*").r }
            words = nodes => { nodes.tail.map(x => getConcept(x._2.concept).toLowerCase/*.replaceAll("[^a-zA-Z0-9\t]","")*/.split("").tail.map(Pattern.quote(_)).mkString("\t?")).mkString("[^a-zA-Z]*").r }
            coRefs = true
        }

        val dateEntity = new SpanAligner(sentence, graph) {
            concept = "date-entity"
            tabSentence = replaceAll(sentence.mkString("\t").toLowerCase,
                Map("january" -> "1",
                    "february" -> "2",
                    "march" -> "3",
                    "april" -> "4",
                    "may" -> "5",
                    "june" -> "6",
                    "july" -> "7",
                    "august" -> "8",
                    "september" -> "9",
                    "october" -> "10",
                    "november" -> "11",
                    "december" -> "12",
                    "[^0-9a-zA-Z\t]" -> ""))
            nodes = node => {
                if (node.children.exists(_._1.matches(":year|:month|:day"))) {
                    ("", node) :: node.children.filter(_._1.matches(":year|:month|:day"))
                } else {
                    List()
                }
            }
            words = nodes => {
                val concepts = nodes.map(x => (x._1, List(x._2.concept)))
                val year = concepts.find(_._1 == ":year").getOrElse(("",List()))._2
                val month = concepts.find(_._1 == ":month").getOrElse(("",List()))._2
                val day = concepts.find(_._1 == ":day").getOrElse(("",List()))._2
                (year ::: month ::: day).permutations.map(_.mkString("\t*0*")).mkString("|").r
            }
            coRefs = true
        }

        val singleConcept = new SpanAligner(sentence, graph) {
            concept = ".*"
            nodes = node => {
                if (alignWord(stemmedSentence, node, wordToSpan) != None) {
                    List(("", node))
                } else {
                    List()
                }
            } 
            spans = nodes => { 
                val Some(index) = alignWord(stemmedSentence, nodes(0)._2, wordToSpan)
                List((index, index+1))
            }
        }

        val fuzzyConcept = new SpanAligner(sentence, graph) {
            concept = ".*"
            nodes = node => {
                if (fuzzyAlign(stemmedSentence, node, wordToSpan) != None) {
                    List(("", node))
                } else {
                    List()
                }
            } 
            spans = nodes => { 
                val Some(index) = fuzzyAlign(stemmedSentence, nodes(0)._2, wordToSpan)
                List((index, index+1))
            }
        }

        val unalignedEntity = new UnalignedConcept(sentence, graph) { concept=".*"; label=":name" }
        val quantity = new UnalignedConcept(sentence, graph) { concept=".*-quantity"; label=":unit" }
        val argOf = new UnalignedConcept(sentence, graph) { concept="person|thing"; label=":ARG.*-of" }
        val governmentOrg = new UnalignedChild(sentence, graph) { concept="government-organization"; label=":ARG0-of" }
        val polarity = new UnalignedChild(sentence, graph) { concept=".*"; label=":polarity"; words="un.*|in.*|il.*" }  // il.* for illegal
        val est = new UnalignedChild(sentence, graph) { concept=".*"; label=":degree"; words=".*est" }
        val er = new UnalignedChild(sentence, graph) { concept=".*"; label=":degree"; words=".*er" }

        addAllSpans(namedEntity, graph, wordToSpan, addCoRefs=false)
        addAllSpans(namedEntity, graph, wordToSpan, addCoRefs=true)
        addAllSpans(dateEntity, graph, wordToSpan, addCoRefs=false)
        addAllSpans(dateEntity, graph, wordToSpan, addCoRefs=true)
        addAllSpans(singleConcept, graph, wordToSpan, addCoRefs=false)
        addAllSpans(fuzzyConcept, graph, wordToSpan, addCoRefs=false)
        try {
            updateSpans(unalignedEntity, graph)
        } catch { case e : Throwable => Unit }
        try {
            updateSpans(quantity, graph)
        } catch { case e : Throwable => Unit }
        try {
            updateSpans(argOf, graph)
        } catch { case e : Throwable => Unit }
        try { updateSpans(governmentOrg, graph) } catch { case e : Throwable => Unit }
        try { updateSpans(polarity, graph) } catch { case e : Throwable => Unit }
        try { updateSpans(est, graph) } catch { case e : Throwable => Unit }
        //try { updateSpans(er, graph) } catch { case e : Throwable => Unit }
        //dateEntities(sentence, graph)
        //namedEntities(sentence, graph)
        //specialConcepts(sentence, graph) // un, in, etc
        //singleConcepts(sentence, graph)
        //learnedConcepts(sentence, graph)

//    spanEntity("""(date-entity)""".r, (node, children) => node :: children.map(_.2), (node, children) => children.map(x => Pattern.quote(getConcept(x._2.concept).toLowerCase)).mkString("[^a-zA-Z]*"))

    // spanEntity(conceptRegex, func_to_produce_nodes, func_to_match_a_span_of_words)

    // newSpan(conceptRegex, func_to_produce_nodes, func_to_match_a_span_of_words, coRefs = true)
    // updateSpan(conceptRegex, pointer_to_span, func_to_produce_nodes, func_to_match_a_span_of_words)

    // newSpan(lcSentence, "name", rep("id.*".r), 

    }

    class SpanAligner(val sentence: Array[String],
                      val graph: Graph) {

        var tabSentence: String = ""
        var concept: String = ""
        var nodes: Node => List[(String, Node)] = x => List()   // function that returns the nodes
        var words: (List[(String,Node)]) => Regex = x => "".r   // function that returns the words
        var coRefs: Boolean = false

        var spans: (List[(String,Node)]) => List[(Int, Int)] = allNodes => {
            val regex = words(allNodes)
            logger(2, "regex: " + regex)
            val matchList = regex.findAllMatchIn(tabSentence).toList
            logger(2, "matchList: " + matchList)
            matchList.map(x => matchToIndices(x, tabSentence))
        }

        def getSpans(node: Node) : List[Span] = {
            logger(2, "Processing node: " + node.concept)
            return node match {
                case Node(_,_,c,_,_,_,_,_) if ((concept.r.unapplySeq(getConcept(c)) != None) && !node.isAligned(graph)) => {
                    logger(2, "Matched concept regex: " + concept)
                    val allNodes = nodes(node)
                    logger(2, "tabSentence: " + tabSentence)
                    if (allNodes.size > 0) {
                        val indices = spans(allNodes) // the default implementation of spans calls words
                        indices.zipWithIndex.map(x => toSpan(x._1, allNodes.map(_._2.id), sentence, graph, x._2 > 0))
                    } else {
                        List()
                    }
                }
            case _ => List()
            }
        }
    }

    class SpanUpdater(val sentence: Array[String],
                      val graph: Graph) {

        var concept: String = ""
        var nodes: Node => List[List[(String, Node)]] = x => List()             // function that returns the nodes
        var spanIndex: List[List[(String, Node)]] => List[Int] = x => List()    // function that returns pointers to spans to update
        var unalignedOnly: Boolean = true

/*        var spans: Node => List[(Int, Int)] = node => {                         // default keeps the words unchanged
            spanIndex(node).map(x => (graph.spans(x).start, graph.spans(x).end))
        } */

        def update(node: Node) {
            logger(2, "SpanUpdater processing node: " + node.concept)
            node match {
                case Node(_,_,c,_,_,_,_,_) if ((concept.r.unapplySeq(getConcept(c)) != None) && (!node.isAligned(graph) || !unalignedOnly)) => {
                    logger(2, "SpanUpdater matched concept regex: " + concept)
                    val allNodes = nodes(node)
                    logger(2, "allNodes = "+allNodes.toString)
                    val updateIndices = spanIndex(allNodes)
                    logger(2, "updateIndices = "+updateIndices.toString)
                    //val wordSpans = spans(node)
                    //for ((spanIndex, (nodes, span)) <- updateIndices.zip(allNodes.zip(wordSpans))) {
                    //    assert(spans == (graph.spans(spanIndex).start, graph.spans(spanIndex).end), "SpanUpdater does not support changing the word span")  // If you want to do this, you must call graph.updateSpan, AND fix up the wordToSpan map
                    for ((spanIndex, nodes) <- updateIndices.zip(allNodes)) {
                        if (nodes.size > 0) {
                            //graph.updateSpan(spanIndex, span._1, span._2, nodes.map(_._2.id), graph.spans(spanIndex).coRef, sentence)
                            logger(2, "Calling updateSpan( "+spanIndex.toString+", "+nodes.map(_._2.id).toString+" )")
                            graph.updateSpan(spanIndex, nodes.map(_._2.id), sentence)
                        } else {
                            logger(2, "Deleting span")
                            graph.updateSpan(spanIndex, 0, 0, List(), false, sentence) // this effectively deletes the span
                        }
                    }
                }
            case _ => Unit
            }
        }
    }

    class UnalignedConcept(override val sentence: Array[String],
                           override val graph: Graph
            ) extends SpanUpdater(sentence, graph) {

        var label: String = ""

        nodes = node => {
            if (node.children.exists(_._1.matches(label))) {
                try {
                    List(("", node) :: graph.spans(node.children.filter(_._1.matches(label))(0)._2.spans(0)).nodeIds.map(x => ("", graph.getNodeById(x))))
                } catch { case e : Throwable => List() }
            } else {
                List()
            }
        }
        spanIndex = nodes => nodes.map(x => x(1)._2.spans.filter(!graph.spans(_).coRef)(0)) // 2nd node of each span
        unalignedOnly = true
    }

    class UnalignedChild(override val sentence: Array[String],
                         override val graph: Graph
            ) extends SpanUpdater(sentence, graph) {

        var label: String = ""
        var words: String = ".*"

        nodes = node => {
            if ((node.children.exists(_._1.matches(label))) && (node.children.filter(_._1.matches(label))(0)._2.spans.size==0) && sentence.slice(graph.spans(node.spans(0)).start, graph.spans(node.spans(0)).end).mkString("\t").matches(words)) {
                try {
                    val child : Node = node.children.filter(_._1.matches(label))(0)._2
                    List(graph.spans(node.spans(0)).nodeIds.map(x => ("", graph.getNodeById(x))) ::: List(("ARG0-of", child)))
                } catch { case e : Throwable => List() }
            } else {
                List()
            }
        }
        spanIndex = nodes => nodes.map(x => x(0)._2.spans(0)) // 1nd node of each span
        unalignedOnly = false
    }

    private def toSpan(startEnd: (Int, Int), nodeIds: List[String], sentence: Array[String], graph: Graph, coRef: Boolean) : Span = {
        // m is a match object which is a match in tabSentence (tabSentence = sentence.mkString('\t'))
        // note: tabSentence does not have to be sentence.mkString('\t') (it could be lowercased, for example)
        assert(nodeIds.size > 0, "Error: matchToSpan passed nodeIds with zero length")
        val (start, end) = startEnd
        val amr = SpanLoader.getAmr(nodeIds, graph)
        val words = sentence.slice(start, end).mkString(" ")
        val span = if (!coRef) {
                Span(start, end, nodeIds, words, amr, coRef)
            } else {
                val node = nodeIds(0)
                Span(start, end, List(node), words, SpanLoader.getAmr(List(node), graph), coRef)
            }
        return span
    }

    private def matchToSpan(m: Regex.Match, nodeIds: List[String], sentence: Array[String], tabSentence: String, graph: Graph, coRef: Boolean) : Span = {
        // m is a match object which is a match in tabSentence (tabSentence = sentence.mkString('\t'))
        // note: tabSentence does not have to be sentence.mkString('\t') (it could be lowercased, for example)
        assert(nodeIds.size > 0, "Error: matchToSpan passed nodeIds with zero length")
        val start = getIndex(tabSentence, m.start)
        val end = getIndex(tabSentence, m.end)+1
        val amr = SpanLoader.getAmr(nodeIds, graph)
        val words = sentence.slice(start, end).mkString(" ")
        val span = if (!coRef) {
                Span(start, end, nodeIds, words, amr, coRef)
            } else {
                val node = nodeIds(0)
                Span(start, end, List(node), words, SpanLoader.getAmr(List(node), graph), coRef)
            }
        return span
    }

    private def matchToIndices(m: Regex.Match, tabSentence: String) : (Int,Int) = {
        return (getIndex(tabSentence, m.start), getIndex(tabSentence, m.end)+1)
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

    private def replaceAll(string: String, map: Map[String, String]) : String = {
        return (map :\ string)((keyValue, s) => s.replaceAll(keyValue._1, keyValue._2))
    }

/****** This stuff was originally in Graph *******/

    private def overlap(span: Span, graph: Graph, wordToSpan: Array[Option[Int]]) : Boolean = {
        var overlap = false
        for (id <- span.nodeIds) {
            overlap = (graph.getNodeById(id).spans.map(x => !graph.spans(x).coRef) :\ overlap)(_ || _)
        }
        for (word <- Range(span.start, span.end)) {
            if (wordToSpan(word) != None) {         // TODO: what about corefs
                overlap = true
            }
        }
        return overlap
    }

    private def addAllSpans(f: AlignSpans2.SpanAligner, graph: Graph, wordToSpan: Array[Option[Int]], addCoRefs: Boolean) {
        def add(node: Node) {
            val spans = if (addCoRefs) {
                f.getSpans(node)
            } else {
                f.getSpans(node).slice(0,1)
            }
            for (span <- spans) {
                if(span.coRef || !overlap(span, graph, wordToSpan)) {
                    graph.addSpan(span)
                    for (i <- Range(span.start, span.end)) {
                        wordToSpan(i) = Some(graph.spans.size-1)
                    }
                }
            }
        }
        graph.doRecursive(graph.root, add)
    }

    private def updateSpans(f: AlignSpans2.SpanUpdater, graph: Graph) {
        graph.doRecursive(graph.root, f.update)
    }

/****** </This stuff was originally in Graph> *******/

    // TODO: everything below can be deleted

    //private val conceptRegex = """-[0-9]+$""".r
    //private val ConceptExtractor = "([a-zA-Z0-9.-]+ *)|\"([^\"]+)\" *".r
    //private val ConceptExtractor = """([a-zA-Z0-9.-]+)\|(?:"([^ ]+)")""".r
    //private val ConceptExtractor = """"?([a-zA-Z0-9.-]+)"?""".r
    //private val ConceptExtractor = """^"?(.+?)(?:-[0-9]+)"?$""".r
    //private val ConceptExtractor = """^"?(.+?)-?[0-9]*"?$""".r // works except for numbers
    def alignWord(stemmedSentence: Array[List[String]], node: Node, alignments: Array[Option[Int]]) : Option[Int] = {
        logger(3,"alignWords: node.concept = "+node.concept)
        var concept = getConcept(node.concept)
        var alignment : Option[Int] = None
        for (i <- Range(0, stemmedSentence.size)) {
            for (word <- stemmedSentence(i)) {
                if (word == concept && alignments(i) == None) {
                    if (alignment != None) {
                        logger(1, "WARNING: Found duplicate match for concept "+node.concept)
                    } else {
                        logger(3,"concept: "+node.concept+" word: "+word)
                        alignment = Some(i)
                    }
                }
            }
        }
        return alignment
    }

    def fuzzyAlign(stemmedSentence: Array[List[String]], node: Node, alignments: Array[Option[Int]]) : Option[Int] = {
        var concept = getConcept(node.concept)
        val size = stemmedSentence.size
        var alignment : Option[Int] = None
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
            for (i <- Range(0, size) if (matchlength(i) == max && alignments(i) == None)) {
                if (alignment == None) {
                    logger(1,"Fuzzy Matcher concept: "+node.concept+" word: "+stemmedSentence(i)(0))
                    alignment = Some(i)
                } else {
                    logger(1, "WARNING: duplicate fuzzy matches for concept "+node.concept)
                }
            }
        }
        return alignment
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
            case "able" => List("possible")
            case "also" => List("include")
            case "anti" => List("oppose","counter")
            case "because" => List("cause")
            //case "biggest" => List("most")  //anything est
            case "but" => List("contrast")
            case "can" => List("possible")
            case "could" => List("possible")
            case "death" => List("die")
            case "French" => List("france","France")
            case "french" => List("france","France")
            case "if" => List("cause")
            case "illegal" => List("law")
            case "may" => List("possible")
            case "no" => List("-")
            case "not" => List("-")
            case "of" => List("include")
            case "speech" => List("speak")
            case "should" => List("recommend")
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

