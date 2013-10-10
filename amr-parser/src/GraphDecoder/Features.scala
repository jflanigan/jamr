package edu.cmu.lti.nlp.amr.GraphDecoder
import edu.cmu.lti.nlp.amr._

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
//    type FeatureFunction = (Int, String, String, Sentence) => FeatureVector
/*    val fftable : Map[String, FeatureFunction] = Map[String, FeatureFunction](
        "unigrams" -> ffunigram,
        "prevtag" -> ffprevtag,
        "1to4" -> ff1to4,
        "prev_next1to4" -> ffprev_next1to4,
        "1to4_conj_prev_next" -> ff1to4_conj_prev_next,
        "prev_tag_conj_1to5" -> ffprev_tag_conj_1to5,
        "prefixes" -> ffprefixes,
        "gazetteer_unigram" -> ffgazetteer_unigram,
        "capital" -> ffcapital,
        "position" -> ffposition
    ) // TODO: error checking on lookup
    val nofast = List("gazetteer_unigram")  // ff that don't support fast lookup

    var prev_t : Int = -1       // For fast features
    var prev_input = Array[Token]()
    var prev_sPrev = ""
    var saved = FeatureVector()

    val feature_functions : List[FeatureFunction] = {
        for { feature <- feature_names
              if !nofast.contains(feature)
        } yield fftable(feature)
    }
    val feature_functions_nofast : List[FeatureFunction] = {
        for { feature <- feature_names
              if nofast.contains(feature)
        } yield fftable(feature)
    }
    //logger(0,feature_names)

    def local(t: Int, sCur: String, sPrev: String, input: Sentence) : FeatureVector = {
        // Calculate the local features given current state, previous state, input
        val feats = FeatureVector()
        for (ff <- feature_functions) {
            feats += ff(t, sCur, sPrev, input)
        }
        return feats
    } */

    def local_score(node1: Node, node2: Node, label: Label, input: Input) : Double = {
        0.0
    }

/*
    def ffunigram(i: Int, sCur: String, sPrev: String, input: Sentence) : FeatureVector = {
        val f = FeatureVector()
        f.fmap += ("Wi=" + input(i).token + ":Ti=" + sCur -> 1.0)
        //f += (input(i-1) -> 1.0)
        //f += (input(i
        return f
    } */
}


