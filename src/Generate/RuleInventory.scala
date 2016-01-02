package edu.cmu.lti.nlp.amr.Generate
import edu.cmu.lti.nlp.amr._
import edu.cmu.lti.nlp.amr.BasicFeatureVector._

import scala.util.matching.Regex
import scala.collection.mutable.{Map, Set, ArrayBuffer}

class RuleInventory(featureNames: Set[String] = Set(), dropSenses: Boolean) {

    val featuresToUse : Set[String] = featureNames.map(x => x match {
        case "source" => Some("corpus")
        case "ruleGivenConcept" => Some("rGc")
        case "nonStopwordCount" => Some("nonStopCount")
        case "nonStopwordCountPronouns" => Some("nonStopCount2")
        case _ => None
    }).filter(x => x != None).map(x => x.get)

    def dropSense(string: String) : String = {
        return string.replaceAll("""-[0-9][0-9]$""","")
    }

    def conceptKey(c: String) = { if (dropSenses) { dropSense(c) } else { c } }

    val conceptCounts : MultiMapCount[String, Unit] = new MultiMapCount()
    val phraseTable : MultiMapCount[String, PhraseConceptPair] = new MultiMapCount()       // Map from concept to PhraseConcepPairs with counts
    val lexRules : MultiMapCount[String, Rule] = new MultiMapCount()            // Map from concept to lexicalized rules with counts
    val abstractRules : MultiMapCount[String, Rule] = new MultiMapCount()       // Map from concept type and args to abstract rules with counts
    val abstractRuleCounts : MultiMapCount[String, Rule] = new MultiMapCount()  // Map from pos to abstract rules with counts
    //val argTableLeft : Map[String, MultiMapCount[String, (String, String)]] = new MultiMapCount()  // Map from pos to map of args to realizations with counts
    //val argTableRight : Map[String, MultiMapCount[String, (String, String)]] = new MultiMapCount() // Map from pos to map of args to realizations with counts
    val argTableLeft : MultiMapCount[(String, String), Arg] = new MultiMapCount()  // Map from (pos, arg) to realizations with counts
    val argTableRight : MultiMapCount[(String, String), Arg] = new MultiMapCount() // Map from (pos, arg) to realizations with counts
    val argsLeft : Map[(String, String), Array[Arg]] = Map()            // Todo: fill in (pos, arg) -> array of realizations
    val argsRight : Map[(String, String), Array[Arg]] = Map()           // make sure there are no gaps
    val conceptArgsLeft : Map[(String, String, String), List[Arg]] = Map()     // (concept, pos, arg) -> array of realizations
    val conceptArgsRight : Map[(String, String, String), List[Arg]] = Map()    // (concept, pos, arg) -> array of realizations

    def load(filename: String) {    // TODO: move to companion object
        conceptCounts.readFile(filename+".conceptcounts", x => x, y => Unit)
        phraseTable.readFile(filename+".phrasetable", x => x, PhraseConceptPair.apply)
        lexRules.readFile(filename+".lexrules", x => x, Rule.apply)
        abstractRules.readFile(filename+".abstractrules", x => x, Rule.apply)
        abstractRuleCounts.readFile(filename+".abstractrulecounts", x => x, Rule.apply)
        createArgTables()
        createArgs()
        createConceptArgs()
    }

    def save(filename: String) {
        writeToFile(filename+".conceptcounts", conceptCounts.toString)
        writeToFile(filename+".phrasetable", phraseTable.toString)
        writeToFile(filename+".lexrules", lexRules.toString)
        writeToFile(filename+".abstractrules", abstractRules.toString)
        writeToFile(filename+".abstractrulecounts", abstractRuleCounts.toString)
    }

    def trainingData(corpus: Iterator[String],
                     posAnno: Array[Annotation[String]]) : Array[(Rule, SyntheticRules.Input)] = {
        var i = 0
        val training_data = new ArrayBuffer[(Rule, SyntheticRules.Input)]()
        for (block <- Corpus.getAMRBlocks(corpus)) {
            logger(0,"**** Processing Block *****")
            logger(0,block)
            val data = AMRTrainingData(block, lowercase)
            val sentence = data.sentence
            //val pos : Array[String] = dependencies(i).split("\n").map(x => x.split("\t")(4))
            val pos =  projectPos(posAnno(i))
            val graph = data.toOracleGraph(clearUnalignedNodes = false)
            training_data ++= extractRules(graph, sentence, pos).filter(x => ruleOk(x._2)).map(x => (x._2, SyntheticRules.Input(x._1, graph)))
            i += 1
        }
        return training_data.toArray
    }

    def extractFromCorpus(corpus: Iterator[String],
                          posAnno: Array[Annotation[String]]) { // TODO: move this constructor to companion object (and rename to fromCorpus)
        //val corpus = Source.fromFile(corpusFilename).getLines
        logger(0, "****** Extracting rules from the corpus *******")

        //val dependencies: Array[String] = (for {
        //        block <- Corpus.splitOnNewline(dependencies)
        //    } yield block.replaceAllLiterally("-LRB-","(").replaceAllLiterally("-RRB-",")").replaceAllLiterally("""\/""","/")).toArray

        var i = 0
        for (block <- Corpus.getAMRBlocks(corpus)) {
            logger(0,"**** Processsing Block *****")
            logger(0,block)
            val data = AMRTrainingData(block, lowercase)
            val sentence = data.sentence
            //val pos : Array[String] = dependencies(i).split("\n").map(x => x.split("\t")(4))
            val pos =  projectPos(posAnno(i))
            //logger(0, "pos = " + pos.mkString(" "))
            val graph = data.toOracleGraph(clearUnalignedNodes = false)
            logger(0,"****** Extracting rules ******")
            for ((_, rule) <- extractRules(graph, sentence, pos) if ruleOk(rule)) {
                lexRules.add(conceptKey(rule.concept.realization.concept) -> rule)
                abstractRules.add(rule.abstractSignature -> Rule.abstractRule(rule))
                abstractRuleCounts.add(rule.concept.realization.headPos -> Rule.abstractRule(rule))
            }
            logger(0,"****** Extracting phrase-concept pairs ******")
            extractPhraseConceptPairs(graph, sentence, pos)
            extractConcepts(graph)
            logger(0,"")
            i += 1
        }
        createArgTables()
        createArgs()
        createConceptArgs()
    }

    def getRules(node: Node) : List[(Rule, FeatureVector)] = {
        val children : List[String] = node.children.map(x => x._1).sorted
        var rules : List[(Rule, FeatureVector)] = List()
        if (!lexRules.map.contains(conceptKey(node.concept))) {
            logger(0, "getRules can't find concept " + node.concept + " in the lexRules table")
        }
        for { (rule, ruleCount) <- lexRules.map.getOrElse(conceptKey(node.concept), Map())
              if rule.concept.realization.amrInstance.children.map(x => x._1).sorted == children
            } {
                val conceptCount = conceptCounts.map.getOrElse(conceptKey(node.concept), Map()).map(x => x._2).sum.toDouble
                val feats = new FeatureVector(Map(
                    "corpus" -> 1.0,
                    "rGc" -> log(ruleCount / conceptCount),
                    // "cGr" -> log(  // need to be able to look up count of rule (for any concept) to do this
                    "nonStopCount" -> rule.nonStopwordCount,
                    "nonStopCount2" -> rule.nonStopwordCount2
                ))
                rules = (rule, feats.slice(feat => featuresToUse.contains(feat))) :: rules
        }
        if (rules.size == 0) {
            logger(0, "getRules couldn't find a matching rule for concept " + node.concept)
        }
        return rules
    }

    def getRealizations(node: Node) : List[(PhraseConceptPair, List[String], FeatureVector)] = {   // phrase, arg labels of children not consumed
        (for { (phrase, phraseCount) <- phraseTable.map.getOrElse(conceptKey(node.concept), Map()) if phrase.matches(node)
                } yield {
            val conceptCount = conceptCounts.map.getOrElse(conceptKey(node.concept), Map()).map(x => x._2).sum.toDouble
            val feats = new FeatureVector(Map(
                "pGc" -> log(phraseCount / conceptCount)
            ))
            (phrase, node.children.map(x => x._1).diff(phrase.amrInstance.children.map(x => x._1)), feats)
        }).toList
        //return phraseTable.map.getOrElse(conceptKey(node.concept), Map()).map(x => (x._1, node.children.map(y => y._1).diff(x._1.amrInstance.children.map(y => y._1)))).toList /*::: passThroughRealizations(node)*/ // TODO: should filter to realizations that could match
    }

    def argOnLeft(conceptRel: PhraseConceptPair, arg: String) : Boolean = {
        // return true if we observed it on the left or we can back off to it on the left, or we didn't observe it at all
        //return conceptArgsLeft.contains((concept.concept, concept.headPos, arg)) || argsLeft.contains((concept.headPos, arg)) || !argsRight.contains((concept.headPos, arg))
        val concept = conceptKey(conceptRel.concept)
        val pos = conceptRel.headPos
        if (conceptArgsLeft.contains((concept, pos, arg))) {            // we observed it on the left
            true
        } else if (!conceptArgsRight.contains(concept, pos, arg)) {     // or we didn't observe it on the right
            if (argsLeft.contains((pos, arg))) {                        // and we can back off to in on the left
                true
            } else if (!argsRight.contains((pos, arg))) {               // or we didn't observe it at all
                true
            } else {
                false                                                   // false because we can back off on the right
            }
        } else {
            false                                                       // false because we observed it on the right
        }
    }

    def argOnRight(conceptRel: PhraseConceptPair, arg: String) : Boolean = {
        // return true if we observed it on the right or we can back off to it on the right, or we didn't observe it at all
        //return conceptArgsRight.contains((concept.concept, concept.headPos, arg)) || argsRight.contains((concept.headPos, arg)) || !argsLeft.contains((concept.headPos, arg))
        val concept = conceptRel.concept
        val pos = conceptRel.headPos
        if (conceptArgsRight.contains((concept, pos, arg))) {           // we observed it on the right
            true
        } else if (!conceptArgsLeft.contains(concept, pos, arg)) {      // or we didn't observe it on the left
            if (argsRight.contains((pos, arg))) {                       // and we can back off to in on the right
                true
            } else if (!argsLeft.contains((pos, arg))) {                // or we didn't observe it at all
                true
            } else {
                false                                                   // false because we can back off on the left
            }
        } else {
            false                                                       // false because we observed it on the left
        }
    }

    def getArgsLeft(conceptRel: PhraseConceptPair, arg: String) : Array[Arg] = {
        val concept = conceptKey(conceptRel.concept)
        val pos = conceptRel.headPos
        //logger(0, "arg: " + arg)
        if (conceptArgsLeft.contains((concept,pos,arg))) {
            conceptArgsLeft((concept,pos,arg)).toArray
        } else {
            //logger(0, "Can't find " + (concept, pos, arg).toString + " in conceptArgsLeft. Returning full list for the pos.")
            argsLeft.getOrElse((pos,arg), Array(Arg.Default(arg)))   // createArgs filters to most common 20 args
        }
    }

    def getArgsRight(conceptRel: PhraseConceptPair, arg: String) : Array[Arg] = {
        val concept = conceptKey(conceptRel.concept)
        val pos = conceptRel.headPos
        //logger(0, "arg: " + arg)
        if (conceptArgsRight.contains((concept,pos,arg))) {
            conceptArgsRight((concept,pos,arg)).toArray
        } else {
            //logger(0, "Can't find " + (concept, pos, arg).toString + " in conceptArgsRight. Returning full list for the pos.")
            argsRight.getOrElse((pos,arg), Array(Arg.Default(arg)))  // createArgs filters to most common 20 args
        }
    }

/*    def opRealizations(node: Node) : List[(PhraseConceptPair, List[String])] = {
        if (node.children.exists(_._2.concept == "name")) {
            nameNode = node.children.filter(x => x._2.concept == "name")(0)
            otherArgs : List[String] = node.children.filter(x => x._2.id != nameNode.id).map(x => x._1)
            val phraseConceptPair = PhraseConceptPair(
            List((phraseConceptPair, otherArgs))
        } else {
            List()
        }
    } */

    def passThroughRealization(node: Node) : List[PhraseConceptPair] = {   // TODO: add features for syntheticRules
        if (node.children.size > 0) {
            if (Set("name", "date-entity").contains(node.concept) || node.concept.matches(".+-.*[a-z]+") || node.children.exists(_._1 == ":name")) {
                // matches list of deleteble concepts
                List(PhraseConceptPair("", node.concept, "NN", "NN"))
            } else /*if (getRealizations(node).size == 0)*/ {
                // concept has no realization
                val pos = if (node.concept.matches(""".*-[0-9][0-9]""")) { "VBN" } else { "NN" }
                List(PhraseConceptPair(node.concept.replaceAll("""-[0-9][0-9]$""",""), node.concept, pos, pos))
            /*} else {
                // it has a realization, so we won't provide one (since the synthetic rule model will provide one)
                List() */
            }
        } else {
            if (node.concept.matches("\".*\"")) {
                // Pass through for string literals
                List(PhraseConceptPair(node.concept.drop(1).init, node.concept, "NNP", "NNP"))
            } else if (getRealizations(node).contains((x: (PhraseConceptPair, List[String])) => x._1.amrInstance.children.size == 0)) {
                // concept has no realization
                if (node.concept.matches(""".*-[0-9][0-9]""")) {
                    // TODO: add inverse rules of WordNet's Morphy: http://wordnet.princeton.edu/man/morphy.7WN.html
                    List(PhraseConceptPair(node.concept.replaceAll("""-[0-9][0-9]$""",""), node.concept, "VBN", "VBN"))
                } else {
                    List(PhraseConceptPair(node.concept, node.concept, "NN", "NN"))
                }
            } else {
                // it has a realization, so we won't provide one (since the synthetic rule model will provide one)
                List()
            }
        }
    }

    def passThroughRules(node: Node) : List[(Rule, FeatureVector)] = {   // TODO: change to passThroughRealizations (and add features for syntheticRules)
        // TODO: filter the features to those in featureNames
        if(node.children.size > 0) {
            /*if (PhraseConceptPair.matches("(person :ARG0-of (have-org-role-91))", node)) {
                if (PhraseConceptPair.matches("(person :ARG0-of (have-org-role-91 :ARG1 <X> :ARG2 <X>) :name <X>)", node)) {
                    List(Rule("(person :ARG0-of have-org-role-91) |||  ||| NN ||| NN ||| "
                } else if (PhraseConceptPair.matches("(person :ARG0-of (have-org-role-91 :ARG1 <X> :ARG2 <X>))", node)) {
                    
                } else if (PhraseConceptPair.matches("(person :ARG0-of (have-org-role-91 :ARG2 <X>))", node)) {

                } else if (PhraseConceptPair.matches("(person :ARG0-of (have-org-role-91 :ARG1 <X>))", node)) {
                    
                }
            } else*/ if (Set("name", "date-entity").contains(node.concept) || node.concept.matches(".+-.*[a-z]+") || node.children.exists(_._1 == ":name") || (node.concept == "and" && node.children.size == 1)) {
                //if (node.concept == "date-entity") {
                //    dateEntity(node)
                //} else {
                val feats = if (node.concept.matches(".+-.*[a-z]+")) {
                    Map("passthrough" -> 1.0, "deletableConcept" -> 1.0)
                } else if (node.concept == "and") {
                    Map("passthrough" -> 1.0, "andDelete" -> 1.0)
                } else if (node.concept == "name" || node.children.exists(_._1 == ":name")) {
                    Map("passthrough" -> 1.0, "nameDelete" -> 1.0)
                } else {
                    Map("passthrough" -> 1.0, "otherDelete" -> 1.0)     // shouldn't get here
                }
                    // matches list of deletable concepts
                    List((Rule(node.children.sortBy(_._1).map(x => Arg("", x._1, "")),
                               ConceptInfo(PhraseConceptPair("", node.concept, "NN", "NN"), 0), "", ""),
                          FeatureVector(feats)))
                //}
            } else if (node.concept == "and") {
                if (node.children.size == 2) {
                    List((Rule(node.children.sortBy(_._1).map(x => Arg("", x._1, "")),
                               ConceptInfo(PhraseConceptPair("and", node.concept, "CC", "CC"), 1), "", ""),
                          FeatureVector(Map("passthrough" -> 1.0, "andPassthrough" -> 1.0))))
                } else {
                    val size = node.children.size
                    List((Rule(node.children.sortBy(_._1).zipWithIndex.map(x => Arg("", x._1._1, if(x._2 >= size - 2) {""} else {","})),
                               ConceptInfo(PhraseConceptPair("and", node.concept, "CC", "CC"), node.children.size - 1), "", ""),
                          FeatureVector(Map("passthrough" -> 1.0, "andPassthrough" -> 1.0))))
                }
            } else /*if (getRealizations(node).size == 0)*/ {
                // concept has no realization
                // TODO: change to passThroughRealizations
                val event = node.concept.matches(""".*-[0-9][0-9]""")
                logger(0, "Adding abstract pass through rule for "+node.concept)
                logger(0, "node.children.size = " + node.children.size)
                logger(0, "node.children = " + node.children.map(x => x._1))
                val pos = if (event) { "VBN" } else { "NN" }
                val abstractSignature : String = (if (event) { "EVENT " } else { "NONEVENT " }) + node.children.map(x => x._1).sorted.mkString(" ")
                logger(0, "abstractSignature = "+abstractSignature)
                if (abstractRules.map.contains(abstractSignature)) {
                    val abstractRule : Rule = abstractRules.map(abstractSignature).maxBy(_._2)._1   // use most common abstract rule
                    val rule = Rule(abstractRule.argRealizations,
                               ConceptInfo(PhraseConceptPair(dropSense(node.concept), node.concept, pos, pos), abstractRule.concept.position), "", "")
                    logger(0, "rule = " + rule.toString)
                    logger(0, "ruleCFG = " + rule.mkRule(withArgLabel=false))
                    List((Rule(abstractRule.argRealizations,
                               ConceptInfo(PhraseConceptPair(dropSense(node.concept), node.concept, pos, pos), abstractRule.concept.position), "", ""),
                        FeatureVector(Map("passthrough" -> 1.0, "abstractPassThrough" -> 1.0))))
                } else {
                    // TODO: backoff synthetic model here
                    List((Rule(node.children.sortBy(_._1).map(x => Arg("", x._1, "")),
                               ConceptInfo(PhraseConceptPair(dropSense(node.concept), node.concept, pos, pos), 0), "", ""),
                        FeatureVector(Map("passthrough" -> 1.0, "withChildrenPassThrough" -> 1.0))))
                }
            /*} else {
                // it has a realization, so we won't provide one (since the synthetic rule model will provide one)
                List() */
            }
        } else {
            if (node.concept.matches("\".*\"")) {
                // Pass through for string literals
                List((Rule(List(), ConceptInfo(PhraseConceptPair(node.concept.drop(1).init, node.concept, "NNP", "NNP"), 0), "", ""), FeatureVector(Map("passthrough" -> 1.0, "stringPassThrough" -> 1.0))))
            } else if (!getRealizations(node).contains((x: (PhraseConceptPair, List[String])) => x._1.amrInstance.children.size == 1)) {
                // concept has no realization
                if (node.concept.matches(""".*-[0-9][0-9]""")) {
                    // TODO: add inverse rules of WordNet's Morphy: http://wordnet.princeton.edu/man/morphy.7WN.html
                    List((Rule(List(), ConceptInfo(PhraseConceptPair(dropSense(node.concept), node.concept, "VBN", "VBN"), 0), "", ""), FeatureVector(Map("eventConceptNoChildrenPassThrough" -> 1.0))))
                } else if(node.concept.matches("""[0-9]+""")) {
                    List((Rule(List(), ConceptInfo(PhraseConceptPair(node.concept, node.concept, "NN", "NN"), 0), "", ""), FeatureVector(Map("passthrough" -> 1.0, "numberPassThrough" -> 1.0))))
                } else {
                    List((Rule(List(), ConceptInfo(PhraseConceptPair(node.concept, node.concept, "NN", "NN"), 0), "", ""), FeatureVector(Map("passthrough" -> 1.0, "nonEventConceptNoChildrenPassThrough" -> 1.0))))
                }
            } else {
                // it has a realization, so we won't provide one (since the synthetic rule model will provide one)
                List()
            }
        }
    }

/*    def dateEntity(node: Node) : List[Rule] = {
        
    } */

/*    def passThroughRealizations(node: Node) : List[(PhraseConceptPair, List[String])] = {
        if(node.children.size > 0) {
                List((PhraseConceptPair("", node.concept, "NN", "NN"), node.children.map(x => x._1)))
        } else {
            if (node.concept.matches("\".*\"")) {
                List((PhraseConceptPair(node.concept.drop(1).init, node.concept, "NNP", "NNP"), List()))
            } else {
                List((PhraseConceptPair(node.concept.replaceAll("""-[0-9][0-9]$""",""), node.concept, "NN", "NN"), List()))
            }
        }
    } */

    private def createArgTables() {
        // Populates argTableLeft and argTableRight
        // Must call extractRules before calling this function
        for ((pos, rules) <- abstractRuleCounts.map) {
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
        for (((pos, arg), countMap) <- argTableLeft.map) {
            //argsLeft((pos, arg)) = countMap.map(x => x._1).toArray
            argsLeft((pos, arg)) = countMap.view.toList.sortBy(x => -x._2).take(20).map(x => x._1).toArray
        }
        for (((pos, arg), countMap) <- argTableRight.map) {
            //argsRight((pos, arg)) = countMap.map(x => x._1).toArray
            argsRight((pos, arg)) = countMap.view.toList.sortBy(x => -x._2).take(20).map(x => x._1).toArray
        }
    }

    private def createConceptArgs() {
        // Populates conceptArgsLeft and conceptArgsRight
        // Must call extractRules before calling this function
        logger(2, "******************* CREATING ARG TABLES *******************")
        for ((concept, rules) <- lexRules.map) {    // lexRules indexes by root concept
            for ((rule, count) <- rules /*if ruleOk(rule, count)*/) {
                logger(2,"rule: "+rule.toString)
                val pos = rule.concept.realization.headPos
                for (arg <- rule.left(rule.argRealizations)) {
                    logger(2,"Adding left: "+(concept, pos, arg.label).toString+" -> " + arg.toString)
                    conceptArgsLeft((concept, pos, arg.label)) = arg :: conceptArgsLeft.getOrElse((concept, pos, arg.label), List())
                    logger(2,"conceptArgsLeft: "+conceptArgsLeft((concept, pos, arg.label)).toString)
                }
                for (arg <- rule.right(rule.argRealizations)) {
                    logger(2,"Adding right: "+(concept, pos, arg.label).toString+" -> " + arg.toString)
                    conceptArgsRight((concept, pos, arg.label)) = arg :: conceptArgsRight.getOrElse((concept, pos, arg.label), List())
                    logger(2,"conceptArgsRight: "+conceptArgsRight((concept, pos, arg.label)).toString)
                }
            }
        }
        for ((key, values) <- conceptArgsLeft) {
            conceptArgsLeft(key) = values.distinct
        }
        for ((key, values) <- conceptArgsRight) {
            conceptArgsRight(key) = values.distinct
        }
        logger(2, "******************* ARG TABLE LEFT *******************")
        for (((concept, pos, label), arg) <- conceptArgsLeft.toArray.sortBy(x => x._1)) {
            logger(2, concept + " " + pos + " ||| " + label + " ||| " + arg.toString)
        }
        logger(2, "******************* ARG TABLE RIGHT *******************")
        for (((concept, pos, label), arg) <- conceptArgsRight.toArray.sortBy(x => x._1)) {
            logger(2, concept + " " + pos + " ||| " + label + " ||| " + arg.toString)
        }
    }

    private def ruleOk(rule : Rule) : Boolean = {
        return true    // TODO: check for unaligned content words
    }

    private def ruleOk(rule : Rule, count: Int) : Boolean = {
        return ruleOk(rule) && count > 0    // could apply filter on low count args
    }

    private def extractConcepts(graph: Graph) {
        // Populates the conceptCounts table
        for (span <- graph.spans) {
            conceptCounts.add(conceptKey(span.amr.concept) -> Unit)
        }
    }

    private def extractPhraseConceptPairs(graph: Graph, sentence: Array[String], pos: Array[String]) {
        // Populates phraseTable
        for (span <- graph.spans) {
            phraseTable.add(conceptKey(span.amr.concept) -> PhraseConceptPair.fromSpan(span, pos))
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

