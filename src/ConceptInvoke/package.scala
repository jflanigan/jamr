package edu.cmu.lti.nlp.amr
import edu.cmu.lti.nlp.amr._
import edu.cmu.lti.nlp.amr.Train._
import edu.cmu.lti.nlp.amr.BasicFeatureVector._

import scala.util.matching.Regex
import scala.collection.mutable.{Map, Set, ArrayBuffer}

package object ConceptInvoke {
    type OptionMap = Map[Symbol, String]

    def Decoder(options: OptionMap, oracle: Boolean = false) : Decoder = {
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

