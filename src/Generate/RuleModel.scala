package edu.cmu.lti.nlp.amr.Generate
import edu.cmu.lti.nlp.amr._

import scala.util.matching.Regex
import scala.collection.mutable.{Map, Set, ArrayBuffer}

case class Rule(lhs: List[String],
                args: Vector[String],
                prefix: String,
                left: List[(String, Int, String)],      // left realization
                lex: String,                            // lexical content
                right: List[(String, Int, String)],     // right realization
                end: String,
                pos: String) {
    def mkRule : String =
        "(X "+(lhs.head :: lhs.tail.sorted).mkString(" ")+") ||| "+rhs
    }
    def rhs : String = {
        (prefix+" "+left.map(x => argStr(x))+" "+lex+" "+right.map(x => argStr(x))+" "+end).replaceAll("^ | $","")
    }
    def argStr(arg: (String, Int, String)) : String = {
        (arg._1+" ["+args(arg._2)"] "+arg._3).replaceAll("^ | $","")
    }
}

case class Phrase(lhs: List[String], words: String, pos: String) {
    def mkRule : String = {
        "(X "+(lhs.head :: lhs.tail.sorted).mkString(" ")+") ||| "+words
    }
}

class RuleModel(options: Map[Symbol, String]) {

    val phraseTable : MultiMapCount[String, Phrase] = new MultiMapCount()       // Map from concept to phrases with counts
    val lexRules : MultiMapCount[String, Rule] = new MultiMapCount()            // Map from concept to rules with counts
    val absRules : MultiMapCount[String, Rule] = new MultiMapCount()            // Map from pos to rules with counts
    val argTableLeft : Map[String, MultiMapCount[String, (String, String)]] = MultiMapCount()  // Map from pos to map of args to realizations with counts
    val argTableRight : Map[String, MultiMapCount[String, (String, String)]] = MultiMapCount() // Map from pos to map of args to realizations with counts

    def trainModel() {
        
    }

    def evaluate() {
        
    }

    def loadModel() {
        
    }

    def createSentenceLevelGrammars() {
        
    }

    def extractRulesFromCorpus() {
        logger(0, "****** Extracting rules from the corpus *******")
        if (!options.contains('corpus)) { println("Must specify corpus file."); sys.exit(1) }
        if (!options.contains('dependencies)) { println("Must specify dependencies file."); sys.exit(1) }

        val dependencies: Array[String] = (for {
                block <- Corpus.splitOnNewline(Source.fromFile(options('dependencies)).getLines())
            } yield block.replaceAllLiterally("-LRB-","(").replaceAllLiterally("-RRB-",")").replaceAllLiterally("""\/""","/")).toArray

        var i = 0
        for { block <- Corpus.splitOnNewline(Source.fromFile(options('corpus).getLines))
              if (block matches "(?:.|\n)*\n\\(?:(?:.|\n)*") } {    // (?:  ) is non-capturing group
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

    def syntheticRules(lhs: List[String]) : List[Rule] = {
        return List()
    }

    def createArgTables {
        for ((pos, rules) <- abstractRules) {
            val tableLeft = MultiMapCount()
            val tableRight = MultiMapCount()
            for ((rule, count) <- rules if ruleOk(rule)) {
                for (x <- rule.left) {
                    val arg = rule.args(x._2)
                    tableLeft.add(arg -> (x._1, x._3), count)
                }
                for (x <- rule.right) {
                    val arg = rule.args(x._2)
                    tableRight.add(arg -> (x._1, x._3), count)
                }
            }
            argTableLeft(pos) = tableLeft
            argTableRight(pos) = tableRight
        }
    }

    def ruleOk(rule : Rule) : Boolean = {
        true
    }

    def extractPhrases(graph: Graph, sentence: Array[String], pos: Array[String]) {
        for (span <- graph.spans) {
            val node = span.amr
            val phrase = Phrase(mkLhs(node),
                                span.words,
                                pos.slice(span.start, span.end).mkString(" "))
            if (conceptTable.contains(node.concept)) {
                val phrases = conceptTable(node.concept).phrases
                phrases(phrase) = phrases.getOrElse(phrase) + 1
            } else {
                conceptTable(node.concept) = ConceptTableEntry(Map(phrase -> 1), Map())
            }
        }
    }

    def mkLhs(node: Node) : List[String] = {
        return "(X " + node.concept + " )" :: node.children.filter(x => x.span == node.span).sortBy(_._1).map(x => "("+x._1.drop(1).toUpperCase.replaceAll("-","_")+" "+printTrees.printRecurseive(x._2)+")")
    }

    def extractRules(graph: Graph,
                     sentence: Array[String],
                     pos : Array[String],
                     spans: Map[String, (Option[Int], Option[Int])],    // map from nodeId to (start, end) in sent
                     spanArray: Array[Boolean],
                     rules : Map[String, Rule]) {

        case class Child(label: String, node: Node, start: Int, end: Int)

        for (span <- graph.spans) {
            val node = graph.getNodeById(span.nodeIds.sorted.apply(0))
            val (ruleStart, ruleEnd) = spans(node.id)
            val children : List[Child] = node.children.filter(x => spans(x._2.id)._1 != None).filter(x => x._2.spans.spans(0) != node.spans(0)).map(x => {val (start, end) = spans(x._2.id); Child(x._1.drop(1).toUpperCase.replaceAll("-",""), x._2, start.get, end.get)}).sortBy(x => x.end)    // notice label => label.drop(1).toUpperCase.replaceAll("-","")
            //logger(1, "children = "+children.toString)
            if (children.size > 0 && !(0 until children.size-1).exists(i => children(i).start > children(i+1).end)) { // check for no overlapping child spans (if so, no rule can be extracted)
                var outsideLower = ruleStart.get
                //do { outsideLower -= 1 } while (outsideLower >= 0 && !spanArray(outsideLower))
                //outsideLower += 1
                var outsideUpper = ruleEnd.get
                //while (outsideUpper < sent.size && !spanArray(outsideUpper)) {
                //    outsideUpper += 1
                //}

                val args : List[Children] = children.sortBy(x => x.label)
                val lowerChildren : Vector[(Children, Int)] = args.zipWithIndex.filter(x => x._1.start < span.start).sortBy(_._1.start).toVector
                val upperChildren : Vector[(Children, Int)] = args.zipWithIndex.filter(x => x._1.start > span.end).start).sortBy(_._1.start).toVector
                val prefix : String = sentence.slice(outsideLower, ruleStart.get)
                val end : String = sentence.slice(myEnd.get, outsideUpper)
                val lex : String = sentence.slice(span.start, span.end).mkString(" ")
                val pos : String = pos.slice(span.start, span.end).mkString(" ")

                val argsList = args.map(x => x.label).toVector
                var left = (0 until lowerChildren.size-1).map(
                    i => ("", x._2, sentence.slice(lowerChildren(i)._2.end, lowerChildren(i+1)._2.start))).toList
                left = left ::: List("", lowerChilren.last._2, sentence.slice(lowerChilren.last._1.end, span.start))
                var right = (1 until upperChildren.size).map(
                    i => (sentence.slice(upperChildren(i-1)._2.end, upperChildren(i)._2.start)), x._2, "").toList
                right = (sentence.slice(span.end, upperChilren.head._1.end), uppserChilren.last._2, "") :: right

                val rule = Rule(mkLhs(node), argsList, prefix, left, lex, right, end, pos)
                lexicalizedRules.add(node.concept, rule)

                val abstractRule = Rule(mkLhs(node), argsList, prefix, left, "", right, end, pos)
                abstractRules.add(pos, abstractRule)

/*
                if (conceptTable.contains(node.concept)) {
                    val rules = conceptTable(node.concept).rules
                    rules(rule) = rules.getOrElse(rule) + 1
                } else {
                    conceptTable(node.concept) = ConceptTableEntry(Map(), Map(rule -> 1))
                }

                if (abstractRules.contains(pos)) {
                    val rules = conceptTable(pos)
                    rules(abstractRule) = rules.getOrElse(abstractRule) + 1
                } else {
                    abstractRules(pos) = Map(abstractRule -> 1)
                } */

/*
                // We will extract the tightest rule (no extra lexical items on either side)
                // and delete spans of lexical items
                val mySpan = graph.spans(node.spans(0))
                val save : Array[String] = sent.slice(mySpan.start, mySpan.end)
                for (i <- Range(mySpan.start, mySpan.end)) {
                    sent(i) = if (i < pos.size) { pos(i) } else { "****" }
                }
                val prefix : List[String] = if (children.size > 0) {
                    sent.slice(outsideLower, children.map(x => x.start).min).toList
                } else {
                    sent.slice(outsideLower, outsideUpper).toList
                }
                var rest : Array[(String, List[String])] = (0 until children.size-1).map(
                    i => (children(i).label, sent.slice(children(i).end, children(i+1).start).toList)).toArray
                val end : (String, List[String]) = if (children.size > 0) {
                    (children(children.size-1).label, sent.slice(children(children.size-1).end, outsideUpper).toList)
                } else {
                    ("", List())
                }
                //logger(1, "prefix = "+prefix.toString)
                //logger(1, "rest = "+rest.toList.toString)
                //logger(1, "end = "+end.toString)

                // The rule is prefix.mkString(" ")+" "+rest.map(x => x._1+" "+x._2.mkString(" ")).mkString(" ")
                val concept = node.concept.replaceAll("""\(""", "-LBR-").replaceAll("""\)""", "-RBR-")
                val labels = children.sortBy(_.label).map(x => "["+x.label.drop(1).toUpperCase.replaceAll("-","")+"]").mkString(" ")
                val ruleARGS = (prefix ::: (rest.toList ::: List(end)).map(x => "["+x._1.drop(1).toUpperCase.replaceAll("-","")+"] "+x._2.mkString(" ")).toList).mkString(" ")
                val rule = (prefix ::: (rest.toList ::: List(end)).zipWithIndex.sortBy(_._1._1).zipWithIndex.sortBy(_._1._2).map(x => "["+(x._2+1).toString+"] "+x._1._1._2.mkString(" ")).toList).mkString(" ")
                //logger(0, "(X (X "+concept+") "+labels+") ||| "+ruleARGS+" ||| "+rule)
                logger(0, "(X (X "+concept+") "+labels+") ||| "+ruleARGS+" ||| "+concept+" ||| "+save.mkString(" ").toLowerCase)
                println("(X (X "+concept+") "+labels+") ||| "+rule)

                for (i <- Range(0, save.size)) {
                    sent(i + mySpan_start) = save(i)
                }
            } */
        }
    }

    def computeSpans(graph: Graph, node: Node, spans: Map[String, (Option[Int], Option[Int])], spanArray: Array[Boolean]) : (Option[Int], Option[Int]) = {
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
        spans(node.id) = (myStart, myEnd)
        return (myStart, myEnd)
    }

}

object RuleModel(options: Map[Symbol, String]) {

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

