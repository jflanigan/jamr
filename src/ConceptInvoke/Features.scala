package edu.cmu.lti.nlp.amr.ConceptInvoke
import edu.cmu.lti.nlp.amr._
import edu.cmu.lti.nlp.amr.Train._
import edu.cmu.lti.nlp.amr.BasicFeatureVector._

import java.io.File
import java.io.FileOutputStream
import java.io.PrintStream
import java.io.BufferedOutputStream
import java.io.OutputStreamWriter
import java.lang.Math
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


/**************************** Feature Functions *****************************/
// TODO: Would this be faster if the ffTable was replaced with boolean variables and 
// lots of if statements?

// TODO: the input to the feature function is just Input and Span.  Change to use Span?

class Features(featureNames: List[String]) {
    var weights = FeatureVector()

    type FeatureFunction = (Input, PhraseConceptPair, Int, Int) => FeatureVector

    val ffTable = Map[String, FeatureFunction](
        "bias" -> ffBias,
        "length" -> ffLength,
        "count" -> ffCount,
        "conceptGivenPhrase" -> ffConceptGivenPhrase,
        "fromNERTagger" -> ffFromNERTagger,
        "fromDateExpr" -> ffFromDateExpr,
        "phraseConceptPair" -> ffPhraseConceptPair,
        "pairWith2WordContext" -> ffPairWith2WordContext
    )

    def ffBias(input: Input, concept: PhraseConceptPair, start: Int, end: Int) : FeatureVector = {
        return FeatureVector(Map("bias" -> 1.0))
    }

    def ffLength(input: Input, concept: PhraseConceptPair, start: Int, end: Int) : FeatureVector = {
        return FeatureVector(Map("len" -> concept.words.size))
    }

    def ffCount(input: Input, concept: PhraseConceptPair, start: Int, end: Int) : FeatureVector = {
        return FeatureVector(Map("N" -> concept.features.count))
    }

    def ffConceptGivenPhrase(input: Input, concept: PhraseConceptPair, start: Int, end: Int) : FeatureVector = {
        return FeatureVector(Map("c|p" -> concept.features.conceptGivenPhrase))
    }

    def ffFromNERTagger(input: Input, concept: PhraseConceptPair, start: Int, end: Int) : FeatureVector = {
        if (concept.features.fromNER) { FeatureVector(Map("ner" -> 1.0))
        } else { FeatureVector(Map("ner" -> 0.0)) }
    }

    def ffFromDateExpr(input: Input, concept: PhraseConceptPair, start: Int, end: Int) : FeatureVector = {
        if (concept.features.fromDateExpr) { FeatureVector(Map("datex" -> 1.0))
        } else { FeatureVector(Map("datex" -> 0.0)) }
    }

    def ffPhraseConceptPair(input: Input, concept: PhraseConceptPair, start: Int, end: Int) : FeatureVector = {
        return FeatureVector(Map("CP="+concept.words.mkString("_")+"=>"+concept.graphFrag.replaceAllLiterally(" ","_") -> 1.0))
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

    var featureFunctions : List[FeatureFunction] = featureNames.map(x => ffTable(x)) // TODO: error checking on lookup

    def localFeatures(input: Input, concept: PhraseConceptPair, start: Int, end: Int) : FeatureVector = {
        // Calculate the local features
        val feats = FeatureVector()
        for (ff <- featureFunctions) {
            feats += ff(input, concept, start, end)
        }
        return feats
    }

    def localScore(input: Input, concept: PhraseConceptPair, start: Int, end: Int) : Double = {
        var score = 0.0
        for (ff <- featureFunctions) {
            score += weights.dot(ff(input, concept, start, end))
        }
        return score
    }

}

