package edu.cmu.lti.nlp.amr
import edu.cmu.lti.nlp.amr.BasicFeatureVector._
import edu.cmu.lti.nlp.amr.ConceptInvoke.PhraseConceptPair

import scala.util.matching.Regex
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer
//import scala.collection.Iterable

import Corpus._

object ExtractConceptTable {
    val usage = """Usage: scala -classpath . edu.cmu.lti.nlp.amr.ExtractConceptTable < aligned_amr_corpus > concept_table"""
    type OptionMap = Map[Symbol, String]

    val implementedFeatures : Set[String] = Set("count", "conceptGivenPhrase", "phraseGivenConcept") 

    def parseOptions(map : OptionMap, list: List[String]) : OptionMap = {
        def isSwitch(s : String) = (s(0) == '-')
        list match {
            case Nil => map
            case "--stage1-features" :: value :: l =>              parseOptions(map + ('stage1Features -> value), l)
            case "--max-training-instances" :: value :: l =>       parseOptions(map + ('maxTrainingInstances -> value), l)
            case "-v" :: value :: l =>                             parseOptions(map + ('verbosity -> value), l)
            case option :: tail => println("Error: Unknown option "+option)
                               sys.exit(1)
        }
    }

    def main(args: Array[String]) {

        if (args.length != 0) { println(usage); sys.exit(1) }

        val options = parseOptions(Map(),args.toList)
        if (options.contains('verbosity)) {
            verbosity = options('verbosity).toInt
        }
        if (!options.contains('stage1Features)) {
            System.err.println("Error: no stage1 features specified during concept table extraction"); sys.exit(1)
        }
        val stage1Features = options('stage1Features).split(",").toList
        val maxCount = options.getOrElse('maxTrainingInstances, "10000").toInt

        val conceptTable : Map[String, List[PhraseConceptPair]] = extract(splitOnNewline(Source.stdin.getLines).toArray, stage1Features, maxCount)
        for { (_, list) <- conceptTable
              concept <- list } {
            println(concept)
        }
    }

    def extract(corpus: Array[String], featureNames: List[String], maxCount: Int) : Map[String, List[PhraseConceptPair]] = {
        // Collect all phrase concept pairs
        val phraseConceptPairs : Map[(List[String], String), (PhraseConceptPair, Int, Int)] = Map()
        val tokenized: Array[Array[String]] = corpus.map(x => Array.empty[String])
        val conceptCounts: Map[String, Int] = Map()
        var i = 0
        for (b <- corpus if b.split("\n").exists(_.startsWith("("))) {  // needs to contain some AMR
            val block = AMRTrainingData(b)
            tokenized(i) = block.sentence
            block.loadSpans()
            for (span <- block.graph.spans) {
                val amr = span.amr.toString.replaceAll(""":op[0-9]*""",":op")
                conceptCounts(amr) = conceptCounts.getOrElse(amr,0) + 1
                val words = span.words.split(" ").toList
                val key = (words, amr)
                if (phraseConceptPairs.contains(key)) {
                    // Could extract more features here, and add them to existing features from the pair
                    var (concept, ruleCount, trainingInstanceCount) = phraseConceptPairs(key)
                    var trainingIndices = concept.trainingIndices
                    if (trainingIndices.head != i) {
                        trainingIndices = i :: trainingIndices    // don't add training example twice
                        trainingInstanceCount += 1                // this # is wrong if > maxCount, but in that case we don't care
                    }
                    if (trainingInstanceCount > maxCount) {
                        trainingIndices = List()
                    }
                    ruleCount += 1
                    phraseConceptPairs(key) = (PhraseConceptPair(words, amr, FeatureVector(), trainingIndices), ruleCount, trainingInstanceCount)
                } else {
                    // Could extract more features here
                    phraseConceptPairs(key) = (PhraseConceptPair(words, amr, FeatureVector(), List(i)), 1, 1)
                }
            }
            i += 1
        }
        
        // Make the map from words to phraseConceptPairs (conceptTable), and initialize phraseCounts to zero
        val phraseCounts : Map[List[String], Int] = Map()
        val conceptTable: Map[String, List[PhraseConceptPair]] = Map()  // map from first word in the phrase to list of PhraseConceptPair
        for ((_, (concept, _, _)) <- phraseConceptPairs) {
            val word = concept.words(0)
            conceptTable(word) = concept :: conceptTable.getOrElse(word, List())
            phraseCounts(concept.words) = 0
        }

        // Count the phrases in the corpus
        for (sentence <- tokenized) {
            for ((word, i) <- sentence.zipWithIndex) {
                val matching = conceptTable.getOrElse(word, List()).filter(x => x.words == sentence.slice(i, i+x.words.size))
                for (concept <- matching) {
                    phraseCounts(concept.words) = phraseCounts(concept.words) + 1
                }
            }
        }

        // Update the conceptTable features
        for { (_, list) <- conceptTable
              concept <- list } {
            val count = phraseConceptPairs((concept.words, concept.graphFrag))._2.toDouble
            if (featureNames.contains("count")) {
                concept.features += FeatureVector(Map("N" -> count))
            }
            if (featureNames.contains("conceptGivenPhrase")) {
                concept.features += FeatureVector(Map("c|p" -> log(count/phraseCounts(concept.words).toDouble)))
            }
            if (featureNames.contains("phraseGivenConcept")) {
                concept.features += FeatureVector(Map("p|c" -> log(count/conceptCounts(concept.graphFrag).toDouble)))
            }
        }

        return conceptTable
    }

/*
    def extractSpans_NoOpN(corpus: Array[String]) : Array[String] = {
        var spans = List[String]()
        for (b <- splitOnNewline(corpus) if b.split("\n").exists(_.startsWith("("))) {  // needs to contain some AMR
            val block = AMRTrainingData(b)
            block.loadSpans()
            for (span <- block.graph.spans) {
                spans = span.words+" ||| "+span.amr.replaceAll(""":op[0-9]*""",":op") :: spans
            }
        }
    }

    def mkConceptTable(corpus: Array[String]) : Map[String, List[PhraseConceptPair]] = {
        val spans : List[(String, Int)] = countUniq(extractSpans_NoOpN(corpus))


    }

    def countUniq[A](list: List[A]) : List[(A, Int)] = {
        (list :\ List[A,Int]())((x, ys) => {    // foldRight
            if (ys.headOption != None && ys.head._1 == x) {
                (x, ys.head._2 + 1) :: ys.tail
            } else {
                (x, 1) :: ys
            }
        })
    } */
}

