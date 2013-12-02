package edu.cmu.lti.nlp.amr.ConceptInvoke
import edu.cmu.lti.nlp.amr._

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

class Features(featureNames: List[String]) {
    var weights = FeatureVector()

    type FeatureFunction = (Input, PhraseConceptPair) => FeatureVector

    val ffTable = Map[String, FeatureFunction](
        "length" -> ffLength,
        "count" -> ffCount,
        "conceptGivenPhrase" -> ffConceptGivenPhrase,
        "fromNERTagger" -> ffFromNERTagger
    )

    def ffLength(input: Input, concept: PhraseConceptPair) : FeatureVector = {
        return FeatureVector(Map("len" -> concept.words.size))
    }

    def ffCount(input: Input, concept: PhraseConceptPair) : FeatureVector = {
        return FeatureVector(Map("N" -> concept.features.count))
    }

    def ffConceptGivenPhrase(input: Input, concept: PhraseConceptPair) : FeatureVector = {
        return FeatureVector(Map("c|p" -> concept.features.conceptGivenPhrase))
    }

    def ffFromNERTagger(input: Input, concept: PhraseConceptPair) : FeatureVector = {
        if (concept.features.fromNER) { FeatureVector(Map("ner" -> 1.0))
        } else { FeatureVector(Map("ner" -> 0.0)) }
    }

    var featureFunctions : List[FeatureFunction] = featureNames.map(x => ffTable(x)) // TODO: error checking on lookup

    def localFeatures(input: Input, concept: PhraseConceptPair) : FeatureVector = {
        // Calculate the local features
        val feats = FeatureVector()
        for (ff <- featureFunctions) {
            feats += ff(input, concept)
        }
        return feats
    }

    def localScore(input: Input, concept: PhraseConceptPair) : Double = {
        var score = 0.0
        for (ff <- featureFunctions) {
            score += weights.dot(ff(input, concept))
        }
        return score
    }

}

