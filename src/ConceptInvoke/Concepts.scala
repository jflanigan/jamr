package edu.cmu.lti.nlp.amr.ConceptInvoke
import edu.cmu.lti.nlp.amr._
import edu.cmu.lti.nlp.amr.Train._
import edu.cmu.lti.nlp.amr.BasicFeatureVector._
import edu.cmu.lti.nlp.amr.ConceptInvoke.PhraseConceptPair._

import scala.collection.mutable.ArrayBuffer
import scala.collection.{mutable => m, immutable => i}

object Concepts {
    val implementedFeatures = m.Set("fromNERTagger", "dateExpression")  // TODO: check
}

class Concepts(options: m.Map[Symbol, String],
               phraseConceptPairs: Array[PhraseConceptPair]) {

// This class contains the code used to invoke concepts.
// Concepts are invoked by calling the invoke() method, which returns a list of all
// the concepts that match a span starting at index i of the tokenized sentence.


    /******* Concept sources to add *********
    - Nominalizations
    - List of -er => person ARG0-of things
    - Entities from large list

    *****************************************/


    val conceptTable: m.Map[String, List[PhraseConceptPair]] = m.Map()  // maps the first word in the phrase to a list of phraseConceptPairs
    for (pair <- phraseConceptPairs) {
        val word = pair.words(0)
        conceptTable(word) = pair :: conceptTable.getOrElse(word, List())
        //logger(2, "conceptTable("+word+") = "+conceptTable(word))
    }

    val conceptSources = options.getOrElse('stage1SyntheticConcepts, "NER,DateExpr").split(",").toSet
    val implementedConceptSources = m.Set("NER","DateExpr","OntoNotes","NEPassThrough","PassThrough","WordNetPassThrough","WordNetPassThrough","verbs","nominalizations")
    assert(conceptSources.filterNot(x => implementedConceptSources.contains(x)).size == 0, "Unknown conceptSources: " + conceptSources.filterNot(x => implementedConceptSources.contains(x)).toList.mkString(", "))

    private var tokens : Array[String] = Array()    // stores sentence.drop(i) (used in the dateEntity code to make it more concise)
    var ontoNotes : m.Set[String] = m.Set()         // could be multi-map instead
    var lemmas : m.Set[String] = m.Set()            // TODO: check for lemma in a large morph-analyzed corpus

    if (options.contains('stage1Predicates)) {
        val Pred = """(.+)-([0-9]+)""".r
        for (predicate <- Source.fromFile(options('stage1Predicates)).getLines) {
            val Pred(verb, sense) = predicate
            ontoNotes += verb
        }
    }

    def invoke(input: Input, i: Int, trainingIndex: Option[Int]) : List[PhraseConceptPair] = {
        // returns a list of all concepts that can be invoke starting at 
        // position i in input.sentence (i.e. position i in the tokenized input)
        // Note: none of the concepts returned have spans that go past the end of the sentence
        val sentence = input.sentence
        if (sentence.size <= i || i < 0) {
            return List()
        }

        var conceptList = if (options.contains('stage1TrainingLeaveOneOut) && trainingIndex != None) {
            conceptTable.getOrElse(sentence(i), List()).filter(
                    x => x.words == sentence.slice(i, i+x.words.size).toList && // TODO: is this case insensitive??
                         x.trainingIndices.filter(j => abs(j - trainingIndex.get) > 20).size > 0 // filter to concepts seen far away
            ).toList
        } else {
            conceptTable.getOrElse(sentence(i), List()).filter(
                    x => x.words == sentence.slice(i, i+x.words.size).toList    // TODO: is this case insensitive??
            ).toList
        }

        if (conceptSources.contains("NER") && options.contains('ner)) {
            conceptList = input.ner.annotation.filter(_.start == i).map(x => namedEntity(input, x)).toList ::: conceptList
            //conceptList = input.ner.annotation.filter(_.start == i).map(x => PhraseConceptPair.entity(input, x)).toList ::: conceptList
        }
        if (conceptSources.contains("DateExpr")) {
            conceptList = dateEntities(input, i) ::: conceptList
        }
        var onlyPassThrough = conceptList.size == 0 // onlyPassThrough indicates the only the pass through rules apply for this span
        if (conceptSources.contains("OntoNotes")) {
            conceptList = ontoNotesLookup(input, i, onlyPassThrough) ::: conceptList
        }
        if (conceptSources.contains("NEPassThrough")) {
            conceptList = NEPassThrough(input, i, onlyPassThrough) ::: conceptList
        }
        if (conceptSources.contains("PassThrough")) {
            conceptList = passThrough(input, i, onlyPassThrough) ::: conceptList
        }
        if (conceptSources.contains("WordNetPassThrough")) {
            conceptList = wordnetPassThrough(input, i, onlyPassThrough) ::: conceptList
        }
        if (conceptSources.contains("verbs")) {
            conceptList = verbs(input, i, onlyPassThrough) ::: conceptList
        }
        if (conceptSources.contains("nominalizations")) {
            conceptList = nominalizations(input, i, onlyPassThrough) ::: conceptList
        }

        // Normalize the concept list so there are no duplicates by adding all their features
        val conceptSet : m.Map[(List[String], String), PhraseConceptPair] = m.Map()
        for (concept <- conceptList.filter(x => x.words == sentence.slice(i, i+x.words.size).toList)) {      // TODO: make this case insensitive?
            val key = (concept.words, concept.graphFrag)
            if (conceptSet.contains(key)) {
                val old = conceptSet(key)
                val feats = new FeatureVector()
                feats += old.features
                feats += concept.features
                val trainingIndices = concept.trainingIndices ::: old.trainingIndices
                conceptSet(key) = PhraseConceptPair(old.words, old.graphFrag, feats, trainingIndices)
            } else {
                conceptSet(key) = concept
            }
        }

        return conceptSet.values.toList
    }

    def ontoNotesLookup(input: Input, i: Int, onlyPassThrough: Boolean) : List[PhraseConceptPair] = {
        val stems = Wordnet.stemmer(input.sentence(i))
        val concepts = stems.filter(stem => ontoNotes.contains(stem)).map(stem => PhraseConceptPair(
            List(input.sentence(i)),
            stem+"-01",         // first sense is most common
            FeatureVector(m.Map("OntoNotes" -> 1.0)),
            List()))
        if (onlyPassThrough) { concepts.map(x => x.features.fmap("OntoNotesOnly") = 1.0 ) }
        return concepts
    }

    def NEPassThrough(input: Input, i: Int, onlyPassThrough: Boolean) : List[PhraseConceptPair] = {
        // TOOD: improve this to check if the words were observed other places
        var concepts = List[PhraseConceptPair]()
        for { j <- Range(1,7)
              if i + j < input.sentence.size
              words = input.sentence.slice(i,i+j).toList 
              if words.filterNot(x => x.matches("[A-Za-z0-9.-]*")).size == 0 } {    // TODO: improve this regex
            concepts = PhraseConceptPair(
                words,
                if (options.contains('stage1Wiki)) {
                    "(thing :wiki - :name (name "+words.map(x => ":op "+x).mkString(" ")+"))"
                } else {
                    "(thing :name (name "+words.map(x => ":op "+x).mkString(" ")+"))"
                },
                FeatureVector(m.Map("NEPassThrough" -> 1.0, "NEPassThrough_len" -> j)),
                List()) :: concepts
        }
        if (onlyPassThrough) { concepts.map(x => x.features.fmap("NEPassThroughOnly") = 1.0 ) }
        return concepts
    }

    def passThrough(input: Input, i: Int, onlyPassThrough: Boolean) : List[PhraseConceptPair] = {
        if(input.sentence(i).matches("[A-Za-z0-9]*")) {     // TODO: improve this regex
            List(PhraseConceptPair(
                List(input.sentence(i)),
                input.sentence(i),
                FeatureVector(m.Map("PassThrough" -> 1.0,
                                    "PassThroughOnly" -> (if(onlyPassThrough) { 1 } else { 0 }) )),
                List()))
        } else { List() }
    }

    def wordnetPassThrough(input: Input, i: Int, onlyPassThrough: Boolean) : List[PhraseConceptPair] = {
        val word = input.sentence(i)
        val stems = Wordnet.stemmer(word)
        // TODO: add stems from large annotated corpus
        if (stems.size > 0) {
            List(PhraseConceptPair(
                List(word),
                stems.minBy(_.size),
                FeatureVector(m.Map("WordnetPassThrough" -> 1.0,
                                    "WordnetPassThroughOnly" -> (if(onlyPassThrough) { 1 } else { 0 }) )),
                List()))
        } else { List() }
    }

    def verbs(input: Input, i: Int, onlyPassThrough: Boolean) : List[PhraseConceptPair] = {
        var concepts = List[PhraseConceptPair]()
        val pos : Array[String] = input.pos.slice(i,i+1)    // NOTE: pos.slice is defined in Annotation.slice
        if (pos.size > 0 && pos(pos.size-1).startsWith("V")) {  // it's a verb
            val word = input.sentence(i)
            val stems = Wordnet.stemmer(word)
            val stem = if (stems.size > 0) { stems.minBy(_.size) } else { word }    // TODO: check in large corpus
            concepts = List(PhraseConceptPair(
                List(word),
                stem+"-00",     // 00 sense for missing predicates
                FeatureVector(m.Map("AddedVerb" -> 1.0,
                                    "AddedVerbOnly" -> (if(onlyPassThrough) { 1 } else { 0 }) )),
                List()))
        }
        return concepts
    }

    def nominalizations(input: Input, i: Int, onlyPassThrough: Boolean) : List[PhraseConceptPair] = {
        // (no change) budget -> budget-01

        // (drop -e in predicate that ends in -ate) proliferate-01 -> proliferation, state-01 -> statement
        // (drop -ify in predicate) intensify-01 -> intensity, ratify-01 -> ratification

        // (drop -ance or -ances) assistance -> assist-01
        // (drop -ment or -ments) development -> develop-02
        // (drop -ing) discriminating -> discriminate-02 (also drop -e)
        // (drop -ion) discrimination -> discriminate-02, discussion -> discuss-01
        // (drop -s) addicts -> addict-01, arrests -> arrest-01
        // (drop -ant or -ants) combatants -> combat-01
        // (drop -ure) seizure -> seize-01, departure -> depart-01, failure -> fail-01 (not always tho: manufacture -> manufacture-01)
        // not as common: (drop -ation) determination -> determine-01 (lots of counter-examples: exaggeration -> exaggerate-01)
        // not common (-ees) employees -> employ-01, attendees -> attend-01

        // -er: parser -> thing :ARG0-of parse-00 (not very common)
        return List()
    }

    // verbalization (modern -> modernize (to make modern), etc, not use in AMR modernize -> modernize-01)

    def namedEntity(input: Input, entity: Entity) : PhraseConceptPair = {
        val Input(_, sentence, notTokenized, _, _, ner, _) = input
        val entityType : String = entity.label match {
            case "PER" => "person"          // also president
            case "ORG" => "organization"    // also company, government-organization, criminal-organization
            case "LOC" => "country"         // also city, world-region, continent, county
            case "MISC" => "thing"         // also treaty, publication, newspaper, product, war
        }
        val (start, end) = ner.getSpan((entity.start, entity.end))         // start and end in ner.snt, which is the tokenized text
        val (notTokStart, notTokEnd) = notTokenized.getSpan((start, end))  // start and end in notTokenized.snt, which is the original untokenized text
        val graphFrag = if (options.contains('stage1Wiki)) {
            "(" + entityType + ":wiki - :name (name " + notTokenized.snt.slice(notTokStart, notTokEnd).map(x => ":op \"" + x.replaceAllLiterally("\"","") + "\"").mkString(" ") + "))" // there should be no " in named entities (TODO: does the AMR style guide say if you can escape them?)
        } else {
            "(" + entityType + " :name (name " + notTokenized.snt.slice(notTokStart, notTokEnd).map(x => ":op \"" + x.replaceAllLiterally("\"","") + "\"").mkString(" ") + "))" // there should be no " in named entities (TODO: does the AMR style guide say if you can escape them?)
        }
        logger(0, "NER Entity: "+graphFrag)
        //logger(1, "(start, end) = "+(start,end))
        //logger(1, "ner.snt = "+ner.snt.toList)
        //logger(1, "ner.tok = "+ner.tok.toList)
        //logger(1, "notTokenized.snt = "+notTokenized.snt.toList)
        //logger(1, "notTokenized.tok = "+notTokenized.tok.toList)
        return PhraseConceptPair(sentence.slice(start, end).toList,
                                 graphFrag,
                                 FeatureVector(m.Map("ner" -> 1.0, "ner_len" -> (end - start))))
    }

    def dateEntities(input: Input, start: Int) : List[PhraseConceptPair] = {
        logger(2, "Finding date entities")
        var list : ArrayBuffer[PhraseConceptPair] = ArrayBuffer()
        tokens = input.sentence.drop(start)
        val string = tokens.mkString("\t")
        var monthRegex = "January|February|March|April|May|June|July|August|September|October|November|December|(?:Jan|Feb|Mar|Apr|Jun|Jul|Aug|Sept?|Oct|Nov|Dec)[.]?"
        monthRegex = monthRegex + "|" + monthRegex.toLowerCase

        // 021114 => (date-entity :day 14 :month 11 :year 2002)
        val SixDigitDate = """(([0-9][0-9])([0-9][0-9])([0-9][0-9]))(?:\t.*)?""".r // (?: ) non-capturing group
        if (SixDigitDate.pattern.matcher(string).matches) {
            list += {
                var SixDigitDate(matching, year, month, day) = string
                if (year.toInt < 40) { year = "20"+year } else { year = "19"+year }
                if (day == "00" && month == "00") { mkYear(matching, year)    // 170000 => (date-entity :year 2017)
                } else if (day == "00") { mkMonthYear(matching, month, year)  // 021100 => (date-entity :month 11 :year 2002)
                } else { mkDayMonthYear(matching, day, month, year) }         // 021114 => (date-entity :day 14 :month 11 :year 2002)
            }
        }

        // 17 July 2003 => (date-entity :day 17 :month 7 :year 2003)
        val DayMonthYear = ("""(([0-9]?[0-9])\t("""+monthRegex+""")\t([0-9][0-9][0-9][0-9]))(?:\t.*)?""").r // (?: ) non-capturing group
        if (DayMonthYear.pattern.matcher(string).matches) {
            list += {
                var DayMonthYear(matching, day, month, year) = string
                mkDayMonthYear(matching, day, month, year)
            }
        }

        // July 2003 => (date-entity :month 7 :year 2003)
        val MonthYear = ("(("+monthRegex+""")\t([0-9][0-9][0-9][0-9]))(?:\t.*)?""").r
        if (MonthYear.pattern.matcher(string).matches) {
            list += {
                var MonthYear(matching, month, year) = string
                mkMonthYear(matching, month, year)
            }
        }

        // July 18 , 2008 => (date-entity :day 18 :month 7 :year 2008)
        val MonthDayYear = ("(("+monthRegex+""")\t?([0-9][0-9]?)\t?,?\t([0-9][0-9][0-9][0-9]))(?:\t.*)?""").r
        if (MonthDayYear.pattern.matcher(string).matches) {
            list += {
                var MonthDayYear(matching, month, day, year) = string
                mkDayMonthYear(matching, day, month, year)
            }
        }

        // 2007-02-27 => (date-entity :day 27 :month 2 :year 2007)
        // 20030106 => (date-entity :day 6 :month 1 :year 2003)
        val EightDigitDate = ("""(([0-9]?[0-9][0-9]?[0-9])\t?[.-]?\t?([0-9][0-9])\t?[.-]?\t?([0-9][0-9]))(?:\t.*)?""").r // (?: ) non-capturing group
        if (EightDigitDate.pattern.matcher(string).matches) {
            list += {
                var EightDigitDate(matching, year, month, day) = string
                mkDayMonthYear(matching, day, month, year)
            }
        }

        // 1713 => (date-entity :year 1713)
        val Year = """(([0-9][0-9][0-9][0-9]))(?:\t.*)?""".r
        if (Year.pattern.matcher(string).matches) {
            list += {
                var Year(matching, year) = string
                mkYear(matching, year)
            }
        }

        // March => (date-entity :month 3)
        val Month = ("(("+monthRegex+"""))(?:\t.*)?""").r
        if (Month.pattern.matcher(string).matches) {
            list += {
                var Month(matching, month) = string
                mkMonth(matching, month)
            }
        }

        return list.toList
    }

    def mkDayMonthYear(matching: String, day: String, month: String, year: String) : PhraseConceptPair = {
        //logger(0, "mkDayMonthYear("+matching+","+day+","+month+","+year+")")
        PhraseConceptPair(tokens.take(matching.count(_ == '\t')+1).toList,
                          "(date-entity :day "+day.toInt.toString+" :month "+monthStr(month)+" :year "+year+")",
                          FeatureVector(m.Map("datex1" -> 1.0, "datex_len" -> (matching.count(_ == '\t') + 1))))
    }

    def mkMonthYear(matching: String, month: String, year: String) : PhraseConceptPair = {
        PhraseConceptPair(tokens.take(matching.count(_ == '\t')+1).toList,
                          "(date-entity :month "+monthStr(month)+" :year "+year+")",
                          FeatureVector(m.Map("datex2" -> 1.0, "datex_len" -> (matching.count(_ == '\t') + 1))))
    }

    def mkMonth(matching: String, month: String) : PhraseConceptPair = {
        PhraseConceptPair(tokens.take(matching.count(_ == '\t')+1).toList,
                          "(date-entity :month "+monthStr(month)+")",
                          FeatureVector(m.Map("datex3" -> 1.0, "datex_len" -> (matching.count(_ == '\t') + 1))))
    }

    def mkYear(matching: String, year: String) : PhraseConceptPair = {
        PhraseConceptPair(tokens.take(matching.count(_ == '\t')+1).toList,
                          "(date-entity :year "+year+")",
                          FeatureVector(m.Map("datex4" -> 1.0, "datex_len" -> (matching.count(_ == '\t') + 1))))
    }

    def monthStr(month: String) : String = {
        if (month.matches("[0-9]*")) {
            month.toInt.toString
        } else {
            month.take(3).toLowerCase match {
                case "jan" => "1"
                case "feb" => "2"
                case "mar" => "3"
                case "apr" => "4"
                case "may" => "5"
                case "jun" => "6"
                case "jul" => "7"
                case "aug" => "8"
                case "sep" => "9"
                case "oct" => "10"
                case "nov" => "11"
                case "dec" => "12"
                case _ => month
            }
        }
    }

}

