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

/****************************** Driver Program *****************************/
object Aligner {

    val usage = """Usage: scala -classpath . edu.cmu.lti.nlp.amr.Aligner -a amr_file -e english_file > alignments"""
    type OptionMap = Map[Symbol, Any]

    def parseOptions(map : OptionMap, list: List[String]) : OptionMap = {
        def isSwitch(s : String) = (s(0) == '-')
        list match {
            case Nil => map
            //case "--train" :: tail =>
            //          parseOptions(map ++ Map('train -> true), tail)
            case "-a" :: value :: tail =>
                      parseOptions(map ++ Map('amrfile -> value), tail)
            case "-e" :: value :: tail =>
                      parseOptions(map ++ Map('englishfile -> value), tail)
            case "-v" :: value :: tail =>
                      parseOptions(map ++ Map('verbosity -> value.toInt), tail)
            //case string :: opt2 :: tail if isSwitch(opt2) => 
            //          parseOptions(map ++ Map('infile -> string), list.tail)
            //case string :: Nil =>  parseOptions(map ++ Map('infile -> string), list.tail)
            case option :: tail => println("Error: Unknown option "+option) 
                               sys.exit(1) 
      }
    }

    def main(args: Array[String]) {

        if (args.length == 0) { println(usage); sys.exit(1) }

        val options = parseOptions(Map(),args.toList)
        if (options.contains('verbosity)) {
            verbosity = options('verbosity).asInstanceOf[Int]
        }
        if (!options.contains('englishfile)) {
            System.err.println("Error: No English source file specified")
            sys.exit(1)
        }
        if (!options.contains('amrfile)) {
            System.err.println("Error: No AMR file specified")
            sys.exit(1)
        }

        val sentences = Source.fromFile(options('englishfile).asInstanceOf[String]).getLines
        val amrs = Source.fromFile(options('amrfile).asInstanceOf[String]).getLines

        for ((sentence, amrstr) <- sentences.zip(amrs)) {
            println(sentence)
            println(amrstr)
            val amr = Graph.parse(amrstr)
            //println(amr)
            val tokenized = sentence.split(" ")
            //println(tokenized.toList)
            val wordAlignments = alignWords(tokenized, amr)
            val spanAlignments = alignSpans(tokenized, amr, wordAlignments)
            logUnalignedConcepts(amr.root)
/*            for ((word, node) <- tokenized.zip(wordAlignments)) {
                node match {
                    case Some(n) => { n.name match {
                        case Some(name) =>  print(word+":("+name+" / "+n.concept+") ")
                        case _ => print(word+":"+n.concept+" ") } }
                    case _ => print(word+" ")
                }
            }
            println() */
            val spans = amr.spans
            //println(spanAlignments.toList)
            //println(spans)
/*            for ((word, spanIndex) <- tokenized.zip(spanAlignments)) {
                spanIndex match {
                    case Some(i) => { spans(i).amr.name match {
                        case Some(name) =>  print(word+":("+name+" / "+spans(i).amr.concept+") ")
                        case _ => print(word+":"+spans(i).amr.concept+" ") } }
                    case _ => print(word+" ")
                }
            }
            println() */
            for ((span, i) <- spans.zipWithIndex) {
                println("Span "+(i+1).toString+":  "+span.words+" => "+span.amr)
            }
            println()
        }
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
    private val ConceptExtractor = """^"?(.+?)-?[0-9]*"?$""".r // works except for numbers
    def alignWords(stemmedSentence: Array[List[String]], node: Node, alignments: Array[Option[Node]]) {
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
        for ((_, child) <- node.relations) {
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
        for ((_, child) <- node.relations) {
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

