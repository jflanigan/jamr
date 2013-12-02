package edu.cmu.lti.nlp.amr.ConceptInvoke
import edu.cmu.lti.nlp.amr._

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

case class PhraseConceptPair(words: List[String], graphFrag: String, features: PhraseConceptFeatures) {

/* The format of the phrase-concept table is
expert ||| (person :ARG1-of expert-41) ||| Count=4 ConceptGivenPhrase=0.3077
*/

    def this(string: String) = this(
        string.split(""" \|\|\| """)(0).split(" ").toList,
        string.split(""" \|\|\| """)(1),
        new PhraseConceptFeatures(string.split(""" \|\|\| """)(2))
    )

}

object PhraseConceptPair {

    def entity(input: Input, entity: Entity) : PhraseConceptPair = {
        val Input(sentence, notTokenized, _, _, ner) = input
        val entityType : String = entity.label match {
            case "PER" => "person"          // also president
            case "ORG" => "organization"    // also company, government-organization, criminal-organization
            case "LOC" => "country"         // also city, world-region, continent, county
            case "MISC" => "thing"         // also treaty, publication, newspaper, product, war
        }
        val (start, end) = ner.getSpan((entity.start, entity.end))    // start and end in ner.snt, which should be the unTokenized text
        val graphFrag = "(" + entityType + " :name (name " + ner.snt.slice(start, end).map(x => ":op \"" + x + "\"").mkString(" ") + "))"
        val (tokStart, tokEnd) = notTokenized.annotationSpan((start, end))
        return PhraseConceptPair(sentence.slice(tokStart, tokEnd).toList, graphFrag, PhraseConceptFeatures(0,0,true))
    }

    var tokens : Array[String] = Array()

    def dateEntity(input: Input, start: Int) : List[PhraseConceptPair] = {
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
        val MonthDayYear = ("(("+monthRegex+""")\t?,?\t([0-9][0-9][0-9][0-9]))(?:\t.*)?""").r
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
        PhraseConceptPair(tokens.take(matching.count(_ == '\t')).toList,
                          "(date-entity :day "+day.toInt.toString+" :month "+monthStr(month)+" :year "+year+")",
                          PhraseConceptFeatures(0, 0, false))
    }

    def mkMonthYear(matching: String, month: String, year: String) : PhraseConceptPair = {
        PhraseConceptPair(tokens.take(matching.count(_ == '\t')).toList,
                          "(date-entity :month "+monthStr(month)+" :year "+year+")",
                          PhraseConceptFeatures(0, 0, false))
    }

    def mkMonth(matching: String, month: String) : PhraseConceptPair = {
        PhraseConceptPair(tokens.take(matching.count(_ == '\t')).toList,
                          "(date-entity :month "+monthStr(month)+")",
                          PhraseConceptFeatures(0, 0, false))
    }

    def mkYear(matching: String, year: String) : PhraseConceptPair = {
        PhraseConceptPair(tokens.take(matching.count(_ == '\t')).toList,
                          "(date-entity :year "+year+")",
                          PhraseConceptFeatures(0, 0, false))
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

