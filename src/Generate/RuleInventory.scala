package edu.cmu.lti.nlp.amr.Generate
import edu.cmu.lti.nlp.amr._

import scala.util.matching.Regex
import scala.collection.mutable.{Map, Set, ArrayBuffer}

class RuleInventory {

    val phraseTable : MultiMapCount[String, PhraseConceptPair] = new MultiMapCount()       // Map from concept to PhraseConcepPairs with counts
    val lexRules : MultiMapCount[String, Rule] = new MultiMapCount()            // Map from concept to lexicalized rules with counts
    val abstractRules : MultiMapCount[String, Rule] = new MultiMapCount()       // Map from pos to abstract rules with counts
    //val argTableLeft : Map[String, MultiMapCount[String, (String, String)]] = new MultiMapCount()  // Map from pos to map of args to realizations with counts
    //val argTableRight : Map[String, MultiMapCount[String, (String, String)]] = new MultiMapCount() // Map from pos to map of args to realizations with counts
    val argTableLeft : MultiMapCount[(String, String), Arg] = new MultiMapCount()  // Map from (pos, arg) to realizations with counts
    val argTableRight : MultiMapCount[(String, String), Arg] = new MultiMapCount() // Map from (pos, arg) to realizations with counts
    val argsLeft : Map[(String, String), Array[Arg]] = Map()    // Todo: fill in (pos, arg) -> array of realizations
    val argsRight : Map[(String, String), Array[Arg]] = Map()   // make sure there are no gaps
 
    def load(filename: String) {    // TODO: move to companion object
        phraseTable.readFile(filename+".phrasetable", x => x, PhraseConceptPair.apply)
        lexRules.readFile(filename+".lexrules", x => x, Rule.apply)
        abstractRules.readFile(filename+".abstractrules", x => x, Rule.apply)
        createArgTables()
        createArgs()
    }

    def save(filename: String) {
        writeToFile(filename+".phrasetable", phraseTable.toString)
        writeToFile(filename+".lexrules", lexRules.toString)
        writeToFile(filename+".abstractrules", abstractRules.toString)
    }

    def trainingData(corpus: Iterator[String],
                     posAnno: Array[Annotation[Array[String]]]) : Array[(Rule, SyntheticRules.Input)] = {
        var i = 0
        val training_data = new ArrayBuffer[(Rule, SyntheticRules.Input)]()
        for (block <- Corpus.getAMRBlocks(corpus)) {
            logger(0,"**** Processsing Block *****")
            logger(0,block)
            val data = AMRTrainingData(block)
            val sentence = data.sentence
            //val pos : Array[String] = dependencies(i).split("\n").map(x => x.split("\t")(4))
            val pos =  projectPos(posAnno(i))
            val graph = data.toOracleGraph(clearUnalignedNodes = false)
            training_data ++= extractRules(graph, sentence, pos).filter(x => ruleOk(x._2)).map(x => (x._2, SyntheticRules.Input(x._1, graph)))
            i += 1
        }
        return training_data.toArray
    }

    def extractFromCorpus(corpus: Iterator[String], posAnno: Array[Annotation[Array[String]]]) { // TODO: move this constructor to companion object (and rename to fromCorpus)
        //val corpus = Source.fromFile(corpusFilename).getLines
        logger(0, "****** Extracting rules from the corpus *******")

        //val dependencies: Array[String] = (for {
        //        block <- Corpus.splitOnNewline(dependencies)
        //    } yield block.replaceAllLiterally("-LRB-","(").replaceAllLiterally("-RRB-",")").replaceAllLiterally("""\/""","/")).toArray

        var i = 0
        for (block <- Corpus.getAMRBlocks(corpus)) {
            logger(0,"**** Processsing Block *****")
            logger(0,block)
            val data = AMRTrainingData(block)
            val sentence = data.sentence
            //val pos : Array[String] = dependencies(i).split("\n").map(x => x.split("\t")(4))
            val pos =  projectPos(posAnno(i))
            //logger(0, "pos = " + pos.mkString(" "))
            val graph = data.toOracleGraph(clearUnalignedNodes = false)
            logger(0,"****** Extracting rules ******")
            for ((_, rule) <- extractRules(graph, sentence, pos) if ruleOk(rule)) {
                lexRules.add(rule.concept.realization.amrInstance.concept -> rule)
                abstractRules.add(rule.concept.realization.headPos -> Rule.abstractRule(rule))
            }
            logger(0,"****** Extracting phrase-concept pairs ******")
            extractPhraseConceptPairs(graph, sentence, pos)
            logger(0,"")
            i += 1
        }
        createArgTables()
        createArgs()
    }

    def getRules(node: Node) : List[Rule] = {
        val children : List[String] = node.children.map(x => x._1).sorted
        return lexRules.map.getOrElse(node.concept, Map()).map(x => x._1).filter(x => x.concept.realization.amrInstance.children.map(y => y._1).sorted == children).toList
    }

    def getRealizations(node: Node) : List[(PhraseConceptPair, List[String])] = {   // phrase, arg labels of children not consumed
        return phraseTable.map.getOrElse(node.concept, Map()).map(x => (x._1, node.children.map(y => y._1).diff(x._1.amrInstance.children.map(y => Label(y._1))))).toList // TODO: should produce a possible realization if not found
    }

    def getArgsLeft(pos_arg: (String, String)) : Array[Arg] = {    // Array[(left, right)]
        return argsLeft.getOrElse(pos_arg, Array(Arg.Default(pos_arg._2)))
    }

    def getArgsRight(pos_arg: (String, String)) : Array[Arg] = {
        return argsRight.getOrElse(pos_arg, Array(Arg.Default(pos_arg._2)))
    }

    private def createArgTables() {
        // Populates argTableLeft and argTableRight
        // Must call extractRules before calling this function
        for ((pos, rules) <- abstractRules.map) {
            for ((rule, count) <- rules if ruleOk(rule, count)) {
                for (arg <- rule.left(rule.argRealizations)) {
                    argTableLeft.add((pos, arg.label) -> arg, count)
                }
                for (arg <- rule.right(rule.argRealizations)) {
                    argTableRight.add((pos, arg.label) -> arg, count)
                }
            }
        }
    }

    private def createArgs() {
        // Populates argsLeft and argsRight
        // Must call createArgTables before calling this function
        for (((pos, arg), countMap) <- argTableLeft.map) {  // TODO: apply a filter on low count args?
            argsLeft((pos, arg)) = countMap.map(x => x._1).toArray
        }
        for (((pos, arg), countMap) <- argTableRight.map) { // TODO: apply a filter on low count args?
            argsRight((pos, arg)) = countMap.map(x => x._1).toArray
        }
    }

    private def ruleOk(rule : Rule) : Boolean = {
        return true    // TODO: check for unaligned content words
    }

    private def ruleOk(rule : Rule, count: Int) : Boolean = {
        return ruleOk(rule) && count > 1
    }

    private def extractPhraseConceptPairs(graph: Graph, sentence: Array[String], pos: Array[String]) {
        // Populates phraseTable
        for (span <- graph.spans) {
            phraseTable.add(span.amr.concept -> PhraseConceptPair.fromSpan(span, pos))
        }
    }

    private def extractRules(graph: Graph,
                             sentence: Array[String],
                             pos : Array[String]) : Iterator[(Node, Rule)] = {

        val spans : Map[String, (Int, Int)] = Map()                 // stores the projected spans for each node
        val spanArray : Array[Boolean] = sentence.map(x => false)   // stores the endpoints of the spans
        computeSpans(graph, graph.root, spans, spanArray)

        (for { span <- graph.spans
              node = graph.getNodeById(span.nodeIds.sorted.apply(0))
              rule = Rule.extract(node, graph, sentence, pos, spans)
              if rule != None
        } yield (node, rule.get)).toIterator
    }

    private def computeSpans(graph: Graph, node: Node, spans: Map[String, (Int, Int)], spanArray: Array[Boolean]) :
            (Option[Int], Option[Int]) = {
        var myStart : Option[Int] = None
        var myEnd : Option[Int] = None
        if (node.spans.size > 0) {
            myStart = Some(graph.spans(node.spans(0)).start)
            myEnd = Some(graph.spans(node.spans(0)).end)
            spanArray(myStart.get) = true
            spanArray(myEnd.get - 1) = true
        }
        for ((_, child) <- node.topologicalOrdering) {
            val (start, end) = computeSpans(graph, child, spans, spanArray)
            if (myStart != None && myEnd != None) {
                if (start != None && end != None) {
                    myStart = Some(min(myStart.get, start.get))
                    myEnd = Some(max(myEnd.get, end.get))
                }
            } else {
                myStart = start
                myEnd = end
            }
        }
        if (myStart != None && myEnd != None) {
            spans(node.id) = (myStart.get, myEnd.get)
        }
        return (myStart, myEnd)
    }

}
/*
object RuleInventory/*(options: Map[Symbol, String])*/ {

    val usage = """Usage: scala -classpath . edu.cmu.lti.nlp.amr.Generate.ExtractSentenceRules --dependencies deps_file --corpus amr_file --decode data """
    type OptionMap = Map[Symbol, Any]

    def parseOptions(map : OptionMap, list: List[String]) : OptionMap = {
        def isSwitch(s : String) = (s(0) == '-')
        list match {
            case Nil => map
            case "-h" :: value :: tail =>                parseOptions(map ++ Map('help -> value.toInt), tail)
            case "-v" :: value :: tail =>                parseOptions(map ++ Map('verbosity -> value.toInt), tail)
            case "--corpus" :: value :: tail =>          parseOptions(map ++ Map('corpus -> value.toInt), tail)
            case "--decode" :: value :: tail =>          parseOptions(map ++ Map('decode -> value.toInt), tail)
            case "--dependencies" :: value :: tail =>    parseOptions(map + ('dependencies -> value), tail)
            case option :: tail => println("Error: Unknown option "+option) 
                               sys.exit(1) 
      }
    }

    def main(args: Array[String]) {
        val options = parseOptions(Map(),args.toList)
        if (options.contains('help)) { println(usage); sys.exit(1) }

        if (options.contains('verbosity)) {
            verbosity = options('verbosity).asInstanceOf[Int]
        }

        if (!options.contains('corpus)) { println("Must specify corpus file."); sys.exit(1) }
        if (!options.contains('decode)) { println("Must specify decode file."); sys.exit(1) }
        if (!options.contains('dependencies)) { println("Must specify dependencies file."); sys.exit(1) }

        val dependencies: Array[String] = (for {
                block <- Corpus.splitOnNewline(Source.fromFile(options('dependencies)).getLines())
            } yield block.replaceAllLiterally("-LRB-","(").replaceAllLiterally("-RRB-",")").replaceAllLiterally("""\/""","/")).toArray

        var i = 0
        for { block <- Corpus.splitOnNewline(Source.fromFile(options('corpus).getLines))
              if (block matches "(.|\n)*\n\\((.|\n)*") } {
            logger(0,"**** Processsing Block *****")
            logger(0,block)
            val data = AMRTrainingData(block)
            val pos : Array[String] = dependencies(i).split("\n").map(x => x.split("\t")(4))
            val graph = data.toOracleGraph(clearUnalignedNodes = false)
            val sentence = data.sentence    // Tokenized sentence
            val spans : Map[String, (Option[Int], Option[Int])] = Map()     // stores the projected spans for each node
            val spanArray : Array[Boolean] = sentence.map(x => false)       // stores the endpoints of the spans
            computeSpans(graph, graph.root, spans, spanArray)
            //logger(0,"spanArray = "+spanArray.zip(sentence).toList.toString)
            logger(0,"****** Extracted rules ******")
            extractRules(graph, graph.root, sentence, pos, spans, spanArray, rules)
            logger(0,"")
            i += 1
        }
    }

}
*/
