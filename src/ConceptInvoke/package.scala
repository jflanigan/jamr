package edu.cmu.lti.nlp.amr
import edu.cmu.lti.nlp.amr._

import java.lang.Math.abs
import java.lang.Math.log
import java.lang.Math.exp
import java.lang.Math.random
import java.lang.Math.floor
import java.lang.Math.min
import java.lang.Math.max
import scala.io.Source
import scala.io.Source.stdin
import scala.io.Source.fromFile
import scala.util.matching.Regex
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer

package object ConceptInvoke {
    type OptionMap = Map[Symbol, String]

    def init(options: OptionMap, oracle: Boolean = false) : Decoder = {
        val stage1Features = options.getOrElse('stage1Features,"length,count").split(",").toList
        logger(0, "Stage1 features = " + stage1Features)

        if (!options.contains('stage1ConceptTable)) {
            System.err.println("Error: No concept table specified"); sys.exit(1)
        }
        val conceptFile = options('stage1ConceptTable)
        val conceptTable = Source.fromFile(conceptFile).getLines.map(x => new PhraseConceptPair(x)).toArray
        val useNER = options.contains('ner)
        if (oracle) {
            new Oracle(stage1Features, conceptTable, useNER)
        } else {
            new Decoder1(stage1Features, conceptTable, useNER)
        }
    }
}

