package edu.cmu.lti.nlp.amr.ConceptInvoke
import edu.cmu.lti.nlp.amr._
import edu.cmu.lti.nlp.amr.Train._
import edu.cmu.lti.nlp.amr.BasicFeatureVector._

import scala.collection.mutable.ArrayBuffer
import scala.collection.{mutable => m, immutable => i}

/**************************** Feature Functions *****************************/
// TODO: Would this be faster if the ffTable was replaced with boolean variables and 
// lots of if statements?

// TODO: the input to the feature function is just Input and Span.  Change to use Span?

class Features(featureNames: List[String], phraseCounts: i.Map[List[String], Int]) {
    var weights = FeatureVector()

    type FeatureFunction = (Input, PhraseConceptPair, Int, Int) => FeatureVector

    val ffTable = m.Map[String, FeatureFunction](
        "bias" -> ffBias,
        "length" -> ffLength,
        "firstMatch" -> ffFirstMatch,
        "phrase" -> ffPhrase,
        "phraseConceptPair" -> ffPhraseConceptPair,
        "pairWith2WordContext" -> ffPairWith2WordContext
    )

    def ffBias(input: Input, concept: PhraseConceptPair, start: Int, end: Int) : FeatureVector = {
        return FeatureVector(m.Map("bias" -> 1.0))
    }

    def ffLength(input: Input, concept: PhraseConceptPair, start: Int, end: Int) : FeatureVector = {
        return FeatureVector(m.Map("len" -> concept.words.size))
    }

    def ffFirstMatch(input: Input, concept: PhraseConceptPair, start: Int, end: Int) : FeatureVector = {
        if (input.sentence.indexOfSlice(concept.words) == start) {
            FeatureVector(m.Map("firstMatch" -> 1.0))
        } else {
            new FeatureVector()
        }
    }

    def ffPhrase(input: Input, concept: PhraseConceptPair, start: Int, end: Int) : FeatureVector = {
        if(phraseCounts.getOrElse(concept.words, 0) > 10) {
            FeatureVector(m.Map("phrase="+concept.words.mkString("_") -> 1.0))
        } else {
            new FeatureVector()
        }
    }

    def ffPhraseConceptPair(input: Input, concept: PhraseConceptPair, start: Int, end: Int) : FeatureVector = {
        if(concept.trainingIndices.size > 10) {
            FeatureVector(m.Map("CP="+concept.words.mkString("_")+"=>"+concept.graphFrag.replaceAllLiterally(" ","_") -> 1.0))
        } else {
            new FeatureVector()
        }
    }

    def ffPairWith2WordContext(input: Input, concept: PhraseConceptPair, start: Int, end: Int) : FeatureVector = {
        val cp = "CP="+concept.words.mkString("_")+"=>"+concept.graphFrag.replaceAllLiterally(" ","_")
        val feats = new FeatureVector()
        if (start > 0) {
            feats.fmap(cp+"+"+"W-1="+input.sentence(start-1)) = 1.0
        }
        if (end < input.sentence.size) {
            feats.fmap(cp+"+"+"W+1="+input.sentence(end)) = 1.0
        }
        return feats
    }

    var featureFunctions : List[FeatureFunction] = featureNames.filter(x => ffTable.contains(x)).map(x => ffTable(x)) // TODO: error checking on lookup
    val unknownFeatures = featureNames.filterNot(x => ffTable.contains(x) || ExtractConceptTable.implementedFeatures.contains(x) || Concepts.implementedFeatures.contains(x))
    assert(unknownFeatures.size == 0, "Unknown stage1 features: "+unknownFeatures.mkString(","))

    def localFeatures(input: Input, concept: PhraseConceptPair, start: Int, end: Int) : FeatureVector = {
        // Calculate the local features
        val feats = FeatureVector()
        for (ff <- featureFunctions) {
            feats += ff(input, concept, start, end)
        }
        feats += concept.features   // add the features in the rule
        return feats
    }

    def localScore(input: Input, concept: PhraseConceptPair, start: Int, end: Int) : Double = {
        var score = 0.0
        for (ff <- featureFunctions) {
            score += weights.dot(ff(input, concept, start, end))
        }
        score += weights.dot(concept.features)  // add the features in the rule
        return score
    }

}

