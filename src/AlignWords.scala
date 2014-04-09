package edu.cmu.lti.nlp.amr

import scala.util.matching.Regex
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer

/****************************** Align Words *****************************/
object AlignWords {

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

