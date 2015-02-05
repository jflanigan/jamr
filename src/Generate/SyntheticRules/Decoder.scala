package edu.cmu.lti.nlp.amr.Generate.SyntheticRules
import edu.cmu.lti.nlp.amr._
import edu.cmu.lti.nlp.amr.Generate._
import edu.cmu.lti.nlp.amr.BasicFeatureVector._

import scala.util.matching.Regex
import scala.collection.mutable.{Map, Set, ArrayBuffer}

case class DecoderResult(rule: Rule, features: FeatureVector, score: Double)

case class Input(node: Node, graph: Graph)

class Decoder(val ruleInventory: RuleInventory) {
    var weights = new FeatureVector()

    val getRealizations = ruleInventory.getRealizations _
    val getArgsLeft = ruleInventory.getArgsLeft _
    val getArgsRight = ruleInventory.getArgsRight _

    def syntheticRules(input: Input) : List[Rule] = {
        // Returns a rule for every concept realization
        var rules : List[Rule] = List()
        var bestRule : Option[Array[Arg]] = None
        var bestScore : Option[Double] = None
        for ((phrase, children) <- getRealizations(input.node)) {
            val DecoderResult(rule, _, _) = decode(phrase, children, input)
            rules = rule :: rules
        }
        return rules
    }

    def decode(conceptRealization: PhraseConceptPair, args: List[String], input: Input) : DecoderResult = {
        val children = args.sorted
        val headPos = conceptRealization.headPos
        val leftTags : List[Array[Arg]] = children.map(label => getArgsLeft((headPos, label)))
        val rightTags : List[Array[Arg]] = children.map(label => getArgsRight((headPos, label)))
        val numArgs = children.size
        var bestResult : Option[DecoderResult] = None
        for (permutation <- (0 until numArgs).permutations) {
            for (i <- 0 to numArgs) { // 0 to numArgs (inclusive)
                //val tagList = leftTags.slice(0,i) ::: List(conceptTag)
                val conceptInfo = ConceptInfo(conceptRealization, i)
                val tagList : List[Array[Arg]] = (
                        permutation.slice(0,i).map(x => leftTags(x)).toList :::
                        List(Array(Arg.CONCEPT)) :::
                        permutation.slice(i,numArgs).map(x => rightTags(x)).toList)  // TODO: use vector instead
                val result = decode(tagList, conceptInfo, input)
                if (bestResult != None && result.score > bestResult.get.score) {
                    bestResult = Some(result)
                }
            }
        }
        return bestResult.get
    }

    def decode(tagList: List[Array[Arg]], concept: ConceptInfo, input: Input) : DecoderResult = {
        def localScore(prev: Arg, cur: Arg, i: Int) : Double = {
            weights.dot(localFeatures(prev, cur, i, concept, input))
        }
        val (resultTags, score) = Viterbi.decode(tagList, localScore _, Arg.START, Arg.STOP)
        val feats = oracle(Rule(resultTags, concept, "", ""), input)
        return DecoderResult(Rule(resultTags, concept, "", ""), feats, weights.dot(feats))
    }

    def oracle(rule: Rule, input: Input) : FeatureVector = {
        val tagList = rule.left(rule.argRealizations).map(x => Array(x)) ::: List(Array(Arg.CONCEPT)) ::: rule.right(rule.argRealizations).map(x => Array(x))
        val prediction = tagList.map(x => 0)
        return oracle(tagList, prediction.toArray, rule.concept, input)
    }

    def oracle(tagList: List[Array[Arg]], prediction: Array[Int], concept: ConceptInfo, input: Input) : FeatureVector = {
        // TODO: can remove this function (only used once)
        // TODO: the code below has some slow copying, maybe change so it's faster (still will be O(n) though)
        val tags : Array[Array[Arg]] = (Array(Arg.START) :: tagList ::: List(Array(Arg.STOP))).toArray
        val pred : Array[Int] = (0 :: prediction.toList ::: List(0)).toArray
        var feats = new FeatureVector()
        for (i <- 0 until tags.size) {
            feats += localFeatures(tags(i-1)(prediction(i-1)), tags(i)(prediction(i)), i, concept, input: Input)
        }
        return feats
    }

    def localFeatures(prev: Arg, cur: Arg, position: Int, concept: ConceptInfo, input: Input) : FeatureVector = {
        // cur.tag = realization tag (from argToTag) (e.g. "_ARG1_'s")
        // cur.arg = argument (e.g. "ARG1", etc)
        val left : Boolean = position < concept.position
        FeatureVector(Map(
            // TODO: add features about headPos (conjoined with distance, etc)
            "r-1="+prev.tag -> 1.0,
            "r="+cur.tag -> 1.0,
            "r-1="+prev.tag+"+"+"r="+cur.tag -> 1.0,
            "A-1="+prev.label+"+"+"A="+cur.label -> 1.0,
            "r="+cur.tag+"+dist" -> abs(concept.position-position),
            "r="+cur.tag+"+s="+(if(left) {"L"} else {"R"}) -> 1.0,
            "r="+cur.tag+"+s="+(if(left) {"L"} else {"R"})+"+dist" -> abs(concept.position-position),
            "A="+cur.label+"+dist" -> abs(concept.position-position),
            "A="+cur.label+"+s="+(if(left) {"L"} else {"R"}) -> 1.0,
            "A="+cur.label+"+s="+(if(left) {"L"} else {"R"})+"+dist" -> abs(concept.position-position)
            ))
    }

}

