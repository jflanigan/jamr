package edu.cmu.lti.nlp.amr.Generate
import edu.cmu.lti.nlp.amr._

import scala.util.matching.Regex
import scala.collection.mutable.{Map, Set, ArrayBuffer}

class RuleModel(options: Map[Symbol, String]) {

    val phraseTable : MultiMapCount[String, PhraseConceptPair] = new MultiMapCount()       // Map from concept to PhraseConcepPairs with counts
    val lexRules : MultiMapCount[String, Rule] = new MultiMapCount()            // Map from concept to lexicalized rules with counts
    val abstractRules : MultiMapCount[String, Rule] = new MultiMapCount()       // Map from pos to abstract rules with counts
    val argTableLeft : Map[String, MultiMapCount[String, (String, String)]] = new MultiMapCount()  // Map from pos to map of args to realizations with counts
    val argTableRight : Map[String, MultiMapCount[String, (String, String)]] = new MultiMapCount() // Map from pos to map of args to realizations with counts

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

    def getRealizations(node: Node) : List[(PhraseConceptPair, List[(String, Node)])] = {   // phrase, children not consumed
        return phraseTable.get.getOrElse(node.concept, List()).map(x => (x, node.children.map(y => (Label(x._1),x._2))))   // TODO: should produce a possible realization if not found
    }

    val argsLeft : Map[(String, String), Array[(String, String)]] = new Map()    // Todo: fill in (pos, arg) -> array of realizations
    val argsRight : Map[(String, String), Array[(String, String)]] = new Map()   // make sure there are no gaps
    
    def getArgsLeft(pos_arg: (String, String)) : Array[(String, String)] = {    // Array[(left, right)]
        return argsLeft.getOrElse(new Array(("","")))   // (left, right), so ("", "") means no words to left or right
    }

    def getArgsRight(pos_arg: (String, String)) : Array[(String, String)] = {
        return argsRight.getOrElse(new Array(("","")))  // (left, right), so ("", "") means no words to left or right
    }

    def argToTag(arg: String, left_right: (String, String)) : Tag = {
        // Example: arg = "ARG1", left_right = ("to", "") => Tag("to_ARG1_", "ARG1")
        return Tag(left_right._1.replaceAllLiterally(" ","_")+"_"+arg+"_"+left_right._2.replaceAllLiterally(" ","_"), arg)
    }

    def conceptTag /*(conceptRealization: ConceptRealization)*/ : Tag = {
        return Tag("<CONCEPT>", "<CONCEPT>")
    }

    case class Tag(tag: String, arg: String)
    case class ConceptInfo(realization: String, headPos: String, position: Int)     // TODO: merge with Phrase?

    def syntheticRules(node: Node, graph: Graph) : List[Rule] = {
        // Returns a rule for every concept realization
        var rules : List[Rule] = List()
        var bestRule : Option[Array[Tag]] = None
        var bestScore : Option[Double] = None
        for ((phrase, children) <- getRealizations(node)) {
            //val leftChildren = children.map((label, node) => argTableLeft.getOrElse(rule.headPos, new MultiMapCount()).get(label).keys.toArray)
            val leftTags : List[Array[Tag]] = children.map((label, node) => getArgsLeft((rule.headPos, label)).map(x => argToTag(label,x)))
            val rightTags : List[Array[Tag]] = children.map((label, node) => getArgsRight((rule.headPos, label)).map(x => argToTag(label,x)))
            val numArgs = children.size
            for (permutation <- (0 until numArgs).permutations) {
                for (i <- 0 to numArgs) { // 0 to numArgs (inclusive)
                    //val tagList = leftTags.slice(0,i) ::: List(conceptTag)
                    val concept = ConceptInfo(phrase.words, phrase.headPOS, i)
                    val tagList : List[Array[Tag]] = (permutation.slice(0,i).map(x => leftTags(x)) ++ Vector(Tag("<CONCEPT>","<CONCEPT>")) ++ permutation.slice(i,numArgs).map(x => rightTags(x))).toList  // TODO: use vector instead
                    val result = decode(tagList, conceptRealization, node, graph)
                    if (bestResult != None && result._3 > bestResult.get._3) {
                        bestResult = Some(result)
                        // Note: this code is correct because getRealizations returns the children sorted by the label, so permutations(x._2) is the correct index to rule.args
                        bestLeftTags = bestResult.get._1.zipWithIndex.slice(0,i).map(x => { val tag = leftTags(permutation(x._2))(x._1); (tag._1, permutations(x._2), tag._2) } )
                        bestRightTags = bestResult.get._1.zipWithIndex.slice(i+1,numArgs+1).map(x => { val tag = rightTags(permutation(x._2))(x._1); (tag._1, permutations(x._2), tag._2) } )  // TODO: fix this (should use left_right, see argToTag)
                        bestConcept = concept
                    }
                }
            }
            //val lhs = 
            val args = children.map(x => x._1).toVector
            val left = bestLeftTags
            val right = bestRightTags
            val rule = Rule(args, "", left, phrase, right, "")
            rules = rule :: rules
        }
        return rules
    }

    def decode(tagList: List[Array[Tag]], concept: ConceptInfo, node: Node, graph: Graph) : (Array[Int], FeatureVector, Double) = {
        val tags : Array[Array[Tag]] = (Array(Tag("<START>","<START>")) :: tagList ::: List((Array(Tag("<STOP>","<STOP>"))))).toArray
        def localScore(state: Viterbi.State) : Double = {
            val i = state.i
            weights.dot(localFeatures(tags(i-1)(state.prev), tags(i)(state.cur), i, concept, node, graph))
        }
        val Viterbi.DecoderResult(tagseq, score) = Viterbi.decode(tags.size, localScore_, i => tags(i).size)
        val feats = oracle(tagList, tagseq.slice(1,tagseq.size-2), concept, node, graph)
        return (tagseq.slice(1,tagseq.size-2), feats, weights.dot(feats))
    }

    def oracle(rule: Rule, concept: ConceptInfo, node: Node, graph: Graph) : FeatureVector = {
        val tagList = rule.left.map(x => (new Array(argToTag(rule.args(x._2), (x._1, x._3))))) ::: List(new Array(conceptTag)) ::: rule.right.map(x => (new Array(argToTag(rule.args(x._2), (x._1, x._3)))))
        val prediction = tagList.map(x => 0)
        return oracle(tagList, prediction, concept, node, graph)
    }

    def oracle(tagList: List[Array[Tag]], prediction: Array[Int], concept: ConceptInfo, node: Node, graph: Graph) : FeatureVector = {
        // TODO: the code below has some slow copying, maybe change so it's faster (still will be O(n) though)
        val tags : Array[Array[Tag]] = (Array(Tag("<START>","<START>")) :: tagList ::: List((Array(Tag("<STOP>","<STOP>"))))).toArray
        val pred : Array[Int] = (0 :: prediction.toList ::: List(0)).toArray
        var feats = new FeatureVector()
        for (i <- 0 until tags.size) {
            feats += localFeatures(tags(i-1)(preds(i-1)), tags(i)(preds(i)), i, concept, node, graph)
        }
        return feats
    }

    def localFeatures(prev: Tag, cur: Tag, position: Int, concept: ConceptInfo, node: Node, graph: Graph) : FeatureVector = {
        // cur.tag = realization tag (from argToTag) (e.g. "_ARG1_'s")
        // cur.arg = argument (e.g. "ARG1", etc)
        val left : Boolean = i < concept.position
        FeatureVector(Map(
            "r-1="+prev.tag -> 1.0,
            "r="+cur.tag -> 1.0,
            "r-1="+prev.tag+"+"+"r="+cur.tag -> 1.0,
            "A-1="+prev.arg+"+"+"A="+cur.arg -> 1.0,
            "r="+cur.tag+"+dist" -> abs(concept.position-position),
            "r="+cur.tag+"+s="+(if(left) {"L"} else {"R"}) -> 1.0,
            "r="+cur.tag+"+s="+(if(left) {"L"} else {"R"})+"+dist" -> abs(concept.position-position),
            "A="+cur.arg+"+dist" -> abs(concept.position-position),
            "A="+cur.arg+"+s="+(if(left) {"L"} else {"R"}) -> 1.0,
            "A="+cur.arg+"+s="+(if(left) {"L"} else {"R"})+"+dist" -> abs(concept.position-position)
            ))
    }

    def createArgTables() {
        // Populates argTableLeft and argTableRight
        // Must call extractRules before calling this function
        for ((pos, rules) <- abstractRules) {
            val tableLeft = MultiMapCount()
            val tableRight = MultiMapCount()
            for ((rule, count) <- rules if ruleOk(rule, count)) {
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

    def ruleOk(rule : Rule, count: Int) : Boolean = {
        return count > 1    // TODO
    }

    def extractPhrasesConceptPairs(graph: Graph, sentence: Array[String], pos: Array[String]) {
        // Populates phraseTable
        for (span <- graph.spans) {
            phraseTable.add(span.amr.concept -> PhraseConceptPair(span, pos))
        }
    }

    def Label(label: String) : String = { 
        return label.drop(1).toUpperCase.replaceAll("-","")
    }

    def extractRules(graph: Graph,
                     sentence: Array[String],
                     pos : Array[String],
                     spans: Map[String, (Option[Int], Option[Int])],    // map from nodeId to (start, end) in sent
                     spanArray: Array[Boolean]) {

        // Populates lexRules and abstractRules

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
                val upperChildren : Vector[(Children, Int)] = args.zipWithIndex.filter(x => x._1.start > span.end).sortBy(_._1.start).toVector
                val prefix : String = sentence.slice(outsideLower, ruleStart.get)
                val end : String = sentence.slice(myEnd.get, outsideUpper)
                val lex : String = sentence.slice(span.start, span.end).mkString(" ")
                val pos : String = pos.slice(span.start, span.end).mkString(" ")
                val headPos : String = pos.slice(span.end-1, span.end)

                val argsList = args.map(x => x.label).toVector
                var left = (0 until lowerChildren.size-1).map(
                    i => ("", x._2, sentence.slice(lowerChildren(i)._2.end, lowerChildren(i+1)._2.start))).toList
                left = left ::: List("", lowerChilren.last._2, sentence.slice(lowerChilren.last._1.end, span.start))
                var right = (1 until upperChildren.size).map(
                    i => (sentence.slice(upperChildren(i-1)._2.end, upperChildren(i)._2.start)), x._2, "").toList
                right = (sentence.slice(span.end, upperChilren.head._1.end), upperChilren.last._2, "") :: right
                val lhs = Rule.mkLhs(node, includeArgs=true)

                val rule = Rule(lhs, argsList, prefix, left, PhraseConceptPair(lex, span.amr.prettyString(0, false, Set.empty[String]), pos, headPos), right, end)
                lexRules.add(node.concept -> rule)

                val abstractRule = Rule(lhs, argsList, prefix, left, PhraseConceptPair("###", span.amr.prettyString(0, false, Set.empty[String]), pos, headPos), right, end)
                abstractRules.add(pos -> abstractRule)
            }
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

object RuleModel/*(options: Map[Symbol, String])*/ {

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

