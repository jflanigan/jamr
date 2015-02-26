package edu.cmu.lti.nlp.amr.Generate.SyntheticRules
import edu.cmu.lti.nlp.amr._
import edu.cmu.lti.nlp.amr.Generate._
import edu.cmu.lti.nlp.amr.BasicFeatureVector._

import scala.util.matching.Regex
import scala.collection.mutable.{Map, Set, ArrayBuffer}

case class DecoderResult(rule: Rule, features: FeatureVector, score: Double)

class Decoder(val ruleInventory: RuleInventory) {
    var weights = new FeatureVector()   // TODO: get rid of this! (add to constructor)

    val getRealizations = ruleInventory.getRealizations _
    val getArgsLeft = ruleInventory.getArgsLeft _
    val getArgsRight = ruleInventory.getArgsRight _

    def syntheticRules(input: Input) : List[Rule] = {
        // Returns a rule for every concept realization
        var rules : List[Rule] = List()
        var bestRule : Option[Array[Arg]] = None
        var bestScore : Option[Double] = None
        for ((phrase, children) <- getRealizations(input.node)) {
            if (children.exists(x => !x.startsWith(":op")) || children.size == 0) {   // Pure op rules we ignore (handled with rule-based system)
                val DecoderResult(rule, _, _) = decode(phrase, children, input)
                rules = rule :: rules
            }
        }
        return rules
    }

    def decode(conceptRealization: PhraseConceptPair, args: List[String], input: Input) : DecoderResult = {
        if (args.size == 0) {
            val rule = Rule(List(), ConceptInfo(conceptRealization, 0), "", "")
            val feats = oracle(rule, input)
            return DecoderResult(rule, feats, weights.dot(feats))
        }
        logger(0, "conceptRealization: "+conceptRealization.toString)
        logger(0, "args: "+args.toString)
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
                if (bestResult == None || result.score > bestResult.get.score) {
                    bestResult = Some(result)
                }
            }
        }
        //logger(0, "Result: "+bestResult.get.rule)
        return bestResult.get
    }

    def decode(tagList: List[Array[Arg]], concept: ConceptInfo, input: Input) : DecoderResult = {
        def localScore(prev: Arg, cur: Arg, i: Int) : Double = {
            weights.dot(localFeatures(prev, cur, i, concept, input))
        }
        //logger(0, "tagList: " + tagList.map(x => x(0)))
        val (resultTags, score) = Viterbi.decode(tagList, localScore _, Arg.START, Arg.STOP)
        //logger(0, "resultTags: " + resultTags.toList)
        val rule = Rule(resultTags.slice(0,concept.position) ::: resultTags.drop(concept.position+1), concept, "", "")
        val feats = oracle(rule, input)
        return DecoderResult(rule, feats, weights.dot(feats))
    }

    def oracle(rule: Rule, input: Input) : FeatureVector = {
        // TODO: the code below has some slow copying, maybe change so it's faster (still will be O(n) though)
        val tags : Array[Arg] = (Arg.START :: rule.left(rule.argRealizations) ::: List(Arg.CONCEPT) ::: rule.right(rule.argRealizations) ::: List(Arg.STOP)).toArray
        var feats = new FeatureVector()
        for (i <- 1 until tags.size) {
            feats += localFeatures(tags(i-1), tags(i), i, rule.concept, input)
        }
        return feats
    }

    def localFeatures(prev: Arg, cur: Arg, position: Int, concept: ConceptInfo, input: Input) : FeatureVector = {
        // cur.tag = realization tag (from argToTag) (e.g. "_ARG1_'s")
        // cur.label = argument (e.g. "ARG1", etc)
        /*********** TODO: the tag level features that don't depend on position can be cached ***************/
        val left : Boolean = position < concept.position
        val pos = concept.realization.headPos
        val c = input.node.concept
        val words = concept.realization.words.replaceAllLiterally(" ","_")
        val side = if(left) {"L"} else {"R"}
        val distance = abs(concept.position-position)
        val leftCount = cur.left.count(_ == ' ')
        val rightCount = cur.right.count(_ == ' ')
        val stopWordCount = splitStr(cur.left + " " + cur.right," ").count(x => stopwords.contains(x))
        FeatureVector(Map(
            // TODO: features of where the concept is, lexical unigrams
            //"R="+cur.tag -> 1.0,
            //"R-1="+prev.tag+"+R="+cur.tag -> 1.0,
            //"A-1="+prev.label+"+A="+cur.label -> 1.0,
            //"R="+cur.tag+"+dist" -> abs(concept.position-position),
            //"R="+cur.tag+"+s="+(if(left) {"L"} else {"R"}) -> 1.0,
            //"R="+cur.tag+"+s="+(if(left) {"L"} else {"R"})+"+dist" -> abs(concept.position-position),
            //"A="+cur.label+"+dist" -> abs(concept.position-position),
            //"A="+cur.label+"+s="+(if(left) {"L"} else {"R"}) -> 1.0,
            //"A="+cur.label+"+s="+(if(left) {"L"} else {"R"})+"+dist" -> abs(concept.position-position),
            //"P="+pos+"+R="+cur.tag -> 1.0,
            //"P="+pos+"+R-1="+prev.tag+"+"+"r="+cur.tag -> 1.0,
            //"P="+pos+"+A-1="+prev.label+"+"+"A="+cur.label -> 1.0,
            //"P="+pos+"+R="+cur.tag+"+dist" -> abs(concept.position-position),
            //"P="+pos+"+R="+cur.tag+"+s="+(if(left) {"L"} else {"R"}) -> 1.0,
            //"P="+pos+"+R="+cur.tag+"+s="+(if(left) {"L"} else {"R"})+"+dist" -> abs(concept.position-position),
            "P="+pos+"+A="+cur.label+"+dist" -> abs(concept.position-position),
            "P="+pos+"+A="+cur.label+"+s="+(if(left) {"L"} else {"R"}) -> 1.0,
            "P="+pos+"+A="+cur.label+"+s="+(if(left) {"L"} else {"R"})+"+dist" -> abs(concept.position-position),
            "W="+words+"+A="+cur.label+"+dist" -> abs(concept.position-position),
            "W="+words+"+A="+cur.label+"+s="+(if(left) {"L"} else {"R"}) -> 1.0,
            "W="+words+"+A="+cur.label+"+s="+(if(left) {"L"} else {"R"})+"+dist" -> abs(concept.position-position),
            //"c="+c+"r="+cur.tag -> 1.0,
            //"W="+words+"+R="+cur.tag -> 1.0
            "W="+words+"+R="+cur.tag+"+s="+side -> 1.0,
            "count" -> (leftCount + rightCount),
            "count_l" -> leftCount,
            "count_r" -> rightCount,
            "SWcount" -> stopWordCount,
            "nonSWcount" -> (leftCount + rightCount - stopWordCount)
            ))
    }

}

