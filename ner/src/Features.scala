package edu.cmu.lti.nlp.ner

import java.io.File
import java.io.FileOutputStream
import java.io.PrintStream
import java.io.BufferedOutputStream
import java.io.OutputStreamWriter
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

class Features(feature_names: List[String]) {
    type FeatureFunction = (Int, String, String, Sentence) => FeatureVector

    val feature_functions : List[FeatureFunction] = feature_names.map(fftable(_))
    
    def local(t: Int, sCur: String, sPrev: String, input: Sentence) : FeatureVector = {
        // Calculate the local features given current state, previous state, input
        val feats = FeatureVector()
        for (ff <- feature_functions) {
            feats += ff(t, sCur, sPrev, input)
        }
        return feats
    }

    val fftable : Map[String, FeatureFunction] = Map[String, FeatureFunction](
        "unigram" -> ffunigram,
        "prevtag" -> ffprevtag
    ) // TODO: error checking on lookup

    def ffunigram(i: Int, sCur: String, sPrev: String, input: Sentence) : FeatureVector = {
        val f = FeatureVector()
        f.fmap += ("Wi=" + input(i) + ":Ti=" + sCur -> 1.0)
        //f += (input(i-1) -> 1.0)
        //f += (input(i
        return f
    }

    def ffprevtag(i: Int, sCur: String, sPrev: String, input: Sentence) : FeatureVector = {
        val f = FeatureVector()
        f.fmap += ("Ti-1=" + sPrev + ":Ti=" + sCur -> 1.0)
        return f
    }

    private def conj(features1: List[String], features2: List[String]) : List[String] = {
        for {feature1 <- features1;
             feature2 <- features2
            } yield feature1 + feature2
    }

    private def fromList(featureList: List[String]) : FeatureVector = {
        val f = FeatureVector()
        for (feature <- featureList) {
            f.fmap += (feature -> (1.0 + f.fmap.getOrElse(feature, 0.0)))
        }
        return f
    }

/*
def ffprefix

def ffsuffix

def ff2digit

def ff4digit

def ffdigit

def digitconj

def ffcap

def gazetteer_unigram

def gazetteer_bigram

def gazetteer_disj

ffmap = Map("Prefixes" => ffprefix)
*/

}

