package edu.cmu.lti.nlp.amr

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
import scala.collection.JavaConversions._
import scala.collection.JavaConverters._ // see http://stackoverflow.com/questions/16162090/how-to-convert-java-util-list-to-scala-list

import java.net.URL

import edu.mit.jwi.RAMDictionary
import edu.mit.jwi.IRAMDictionary
import edu.mit.jwi.data.ILoadPolicy
import edu.mit.jwi.item.POS
import edu.mit.jwi.morph.WordnetStemmer

object Wordnet {

    private val wnhome : String = System.getenv("WNHOME")
    private val path : String = wnhome + File.separator + "dict"
    private val url : URL = new URL("file", null, path)
    private val dict : IRAMDictionary = new RAMDictionary(url, ILoadPolicy.NO_LOAD)
    dict.open
    private val wordnetStemmer : WordnetStemmer = new WordnetStemmer(dict)

    def stemmer(word: String) : List[String] = {
        var stems = List[String]()
        for (pos <- POS.values) {
            stems ++= wordnetStemmer.findStems(word, pos)
        }
        return stems.distinct.sorted
    }

    def stemmer(word: String, pos: POS) : List[String] = {
        return wordnetStemmer.findStems(word, pos).asScala.toList
    }

}

