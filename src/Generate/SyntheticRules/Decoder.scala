package edu.cmu.lti.nlp.amr.Generate
import edu.cmu.lti.nlp.amr._
import edu.cmu.lti.nlp.amr.BasicFeatureVector._

import scala.util.matching.Regex
import scala.collection.mutable.{Map, Set, ArrayBuffer}

class Decoder(val ruleInventory: RuleInventory) {
    var weights = new FeatureVector()

    val getRealizations = ruleInventory.getRealizations_
    val getArgsLeft = ruleInventory.getArgsLeft_
    val getArgsRight = ruleInventory.getArgsRight_

    case class Tag(tag: String, arg: String, realization: (String, String))
    case class ConceptInfo(realization: PhraseConceptPair, position: Int)     // TODO: merge with Phrase?

    def argToTag(arg: String, left_right: (String, String)) : Tag = {
        // Example: arg = "ARG1", left_right = ("to", "") => Tag("to_ARG1_", "ARG1")
        return Tag(left_right._1.replaceAllLiterally(" ","_")+"_"+arg+"_"+left_right._2.replaceAllLiterally(" ","_"), arg, left_right)
    }

    val conceptTag = Tag("<CONCEPT>", "<CONCEPT>", ("", ""))

    def getTagsLeft(pos: String, arg: String) : Tag = argToTag(arg, getArgsLeft((pos, arg)))
    def getTagsRight(pos: String, arg: String) : Tag = argToTag(arg, getArgsRight((pos, arg)))

    def syntheticRules(node: Node, graph: Graph) : List[Rule] = {
        // Returns a rule for every concept realization
        var rules : List[Rule] = List()
        var bestRule : Option[Array[Tag]] = None
        var bestScore : Option[Double] = None
        for ((phrase, children) <- getRealizations(node)) {
            val DecoderResult(rule, _, _) = decode(phraseConceptPair, children, node, graph)
            rules = rule :: rules
        }
        return rules
    }

    case class DecoderResult(rule: Rule, features: FeatureVector, score: Double)

    def decode(oracleRule: Rule, node: Node, graph: Graph) : DecoderResult = {
        return decode(rule.concept, rule.args, node, graph)
    }

    def decode(conceptRealization: PhraseConceptPair, args: Vector[String], node: Node, graph: Graph) : DecoderResult = {
        val children = args.sorted
        val headPos = conceptRealization.headPos
        val leftTags : List[Array[Tag]] = children.map(label => getArgsLeft((headPos, label)).map(x => argToTag(label,x)))
        val rightTags : List[Array[Tag]] = children.map(label => getArgsRight((headPos, label)).map(x => argToTag(label,x)))
        val numArgs = children.size
        var bestResult : Option[(List[Tag], FeatureVector, Double)] = None
        var bestConcept : Option[ConceptInfo] = None    // we only need best position
        for (permutation <- (0 until numArgs).permutations) {
            for (i <- 0 to numArgs) { // 0 to numArgs (inclusive)
                //val tagList = leftTags.slice(0,i) ::: List(conceptTag)
                val concept = ConceptInfo(conceptRealization, i)
                val tagList : List[Array[Tag]] = (
                        permutation.slice(0,i).map(x => leftTags(x)) ++
                        Vector(Tag("<CONCEPT>","<CONCEPT>", ("",""))) ++
                        permutation.slice(i,numArgs).map(x => rightTags(x))).toList  // TODO: use vector instead
                val result = decode2(tagList, conceptRealization, node, graph)
                if (bestResult != None && result._3 > bestResult.get._3) {
                    bestResult = Some(result)
                    bestConcept = Some(concept)
                }
            }
        }
        val argRealizations = bestResult.get._1.zipWithIndex.map((tag, i) => (tag.realization._1, i, tag.realization._2))
        val c = bestConcept.position
        val left = argRealizations.slice(0, c)
        val right = argRealizations.slice(c+1, numArgs+1)
        return DecoderResult(Rule(children, "", left, conceptRealization, right, ""), bestResult._2, bestResult._3)
    }

    def decode(tagList: List[Array[Tag]], concept: ConceptInfo, node: Node, graph: Graph) : (List[Tag], FeatureVector, Double) = {
        val tags : Array[Array[Tag]] = (Array(Tag("<START>","<START>", ("",""))) :: tagList ::: List((Array(Tag("<STOP>","<STOP>", ("",""))))).toArray
        def localScore(state: Viterbi.State) : Double = {
            val i = state.i
            weights.dot(localFeatures(tags(i-1)(state.prev), tags(i)(state.cur), i, concept, node, graph))
        }
        val Viterbi.DecoderResult(tagseq, score) = Viterbi.decode(tags.size, localScore_, i => tags(i).size)
        val feats = oracle(tagList, tagseq.slice(1,tagseq.size-2), concept, node, graph)
        val resultIndexes : List[Int] = tagseq.toList       // indexes into tagList's arrays
        val resultTags : List[Tag] = resultIndexes.zip(tagList).map((i,tags) => tags(i))  // the actual tags (+1 because START tag)
        return (resultTags.slice(1,tagseq.size-2), feats, weights.dot(feats))
    }

    def oracle(rule: Rule, node: Node, graph: Graph) : FeatureVector = {
        val concept = ConceptInfo(rule.concept, rule.left.size)
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
            // TODO: add reatures about headPos (conjoined with distance, etc)
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

}

