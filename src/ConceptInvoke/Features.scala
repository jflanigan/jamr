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

    /******* Features to add *********
    - Fragtype feature (is it an event, named entity, number, string constant, other fragment
    - Fragtype and POS tag
    - Concept bigrams, and concepts w/o sense tags
    - Edge type to named entity

    **********************************/

    val ffTable = m.Map[String, FeatureFunction](
        "bias" -> ffBias,
        "length" -> ffLength,
        "firstMatch" -> ffFirstMatch,
        "numberIndicator" -> ffNumberIndicator,
        "badConcept" -> ffBadConcept,
        "sentenceMatch" -> ffSentenceMatch,
        "andList" -> ffAndList,
        "pos" -> ffPOS,
        "posEvent" -> ffPOSEvent,
        "phrase" -> ffPhrase,
        "phraseConceptPair" -> ffPhraseConceptPair,
        "phraseConceptPairPOS" -> ffPhraseConceptPairPOS,
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

    def ffNumberIndicator(input: Input, concept: PhraseConceptPair, start: Int, end: Int) : FeatureVector = {
        if (concept.words.size == 1 && concept.words.head.matches("[0-9]*") && concept.words.head == concept.graphFrag) {
            FeatureVector(m.Map("numIndicator" -> 1.0))
        } else {
            new FeatureVector()
        }
    }

    def ffBadConcept(input: Input, concept: PhraseConceptPair, start: Int, end: Int) : FeatureVector = {
        if (concept.graphFrag.matches("[A-Za-z]*") && concept.graphFrag.size <= 2) {
            FeatureVector(m.Map("badConcept" -> 1.0))
        } else {
            new FeatureVector()
        }
    }

    def ffSentenceMatch(input: Input, concept: PhraseConceptPair, start: Int, end: Int) : FeatureVector = {
        if (input.sentence.size == concept.words.size) {
            FeatureVector(m.Map("sentenceMatch" -> 1.0))
        } else {
            new FeatureVector()
        }
    }

    def ffAndList(input: Input, concept: PhraseConceptPair, start: Int, end: Int) : FeatureVector = {
        if (input.sentence(start) == ";" && input.sentence.mkString(" ").matches("[^;]+(?: ; .*)+[^.!?]$")) {    // ; separated list
            if (start == input.sentence.indexOf(";") && end == start + 1) {
                FeatureVector(m.Map("andList" -> 1.0))
            } else {
                FeatureVector(m.Map("andListNot" -> 1.0))
            }
        } else {
            new FeatureVector()
        }
    }

    def ffPOS(input: Input, concept: PhraseConceptPair, start: Int, end: Int) : FeatureVector = {
        return FeatureVector(m.Map("POS=" + input.pos.slice(start, end).mkString("_") -> 1.0))
    }
    
    def ffPOSEvent(input: Input, concept: PhraseConceptPair, start: Int, end: Int) : FeatureVector = {
        return FeatureVector(m.Map("POS=" + input.pos.slice(start, end).mkString("_")+"+EVENT="+(if(concept.graphFrag.matches(".*-[0-9][0-9]")) { "T" } else { "F" })  -> 1.0))
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

    def ffPhraseConceptPairPOS(input: Input, concept: PhraseConceptPair, start: Int, end: Int) : FeatureVector = {
        if(concept.trainingIndices.size > 3) {
            FeatureVector(m.Map("CP="+concept.words.mkString("_")+"+POS="+input.pos.slice(start, end).mkString("_")+"=>"+concept.graphFrag.replaceAllLiterally(" ","_") -> 1.0))
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

