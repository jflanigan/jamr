package edu.cmu.lti.nlp.amr
import edu.cmu.lti.nlp.amr._
import scala.language.implicitConversions

import scala.util.matching.Regex
import scala.collection.mutable.{Map, Set, ArrayBuffer}

package object Generate {
    var lowercase : Boolean = false
    def Label(label: String) : String = {
        return label.drop(1).toUpperCase.replaceAll("-","_")
    }
    def dropSense(string: String) : String = {
        return string.replaceAll("""-[0-9][0-9]$""","")
    }
    //def labelStr(label: String) = Label(label)
    //class Label(str: String)
    //object Label {
    //    apply(str: String) : Label = { str.drop(1).toUpperCase.replaceAll("-","_") }
    //}
    def escape(str: String, esc: Char = '\\') : String = {
        return str.replaceAllLiterally(esc.toString, esc.toString + esc.toString)
    }
    def escape(str: String, esc: String) : String = {
        var s = str
        for (c <- esc) {
            s = s.replaceAllLiterally(esc.toString, esc.toString + esc.toString)
        }
        return s
    }
    def unEscape(str: String, esc: Char = '\\') : String = {    // unescapes to a tab
        return str.replaceAllLiterally(esc.toString, "\t").replaceAllLiterally("\t\t", esc.toString)
    }
    def unEscape(str: String, esc: String) : String = {         // unescapes to a tab
        var s = str
        for (c <- esc) {
            s = s.replaceAllLiterally(esc.toString, "\t")
            s = s.replaceAllLiterally("\t\t", esc.toString)
        }
        return s
    }
    def unEscapeArray(str: String, esc: Char) : Array[String] = {
        return unEscape(str, esc).splitStr("\t")
    }
    def projectPos(posAnno: Annotation[String]) : Array[String] = {
        val sentence : Array[String] = posAnno.snt // tokenized sentence
        val pos = (0 until sentence.size).map(i => {
            val span = posAnno.annotationSpan(i,i+1)
            val posList = posAnno.annotation.slice(span._1, span._2)    // should always return a non-empty array
            posList.last    // take the last one (works well in English)
        }).toArray
        return pos
    }

    // stopwordCount() and nonStopwordCount() use words in the 'stopwordsSmall' set.
    // stopwordCount2() and nonStopwordCount2() use words in the 'stopwords' set.
    // 'stopwords' contains all the words in 'stopwordSmall', but also contains pronouns and demonstratives
    def nonStopwordCount(string: String) : Int = {
        string.splitStr(" ").count(x => !stopwordsSmall.contains(x))
    }

    def stopwordCount(string: String) : Int = {
        string.splitStr(" ").count(x => stopwordsSmall.contains(x))
    }

    def nonStopwordCount2(string: String) : Int = {
        string.splitStr(" ").count(x => !stopwords.contains(x))
    }

    def stopwordCount2(string: String) : Int = {
        string.splitStr(" ").count(x => stopwords.contains(x))
    }

    def badStopwordCount(string: String) : Int = {
        string.splitStr(" ").count(x => badStopwords.contains(x))
    }

    def negationWordCount(string: String) : Int = {
        val negationWords = Set("no", "not", "n't")
        string.splitStr(" ").count(x => negationWords.contains(x))
    }

    // Get these words by looking at most common unigram missed in synthetic rules
    // despite
    // receiving
    // made
    // prove

    val stopwordsVerySmall = Set(
        "",
        ",",
        "'s",
        "a",
        //"about",
        //"above",
        //"after",
        //"again",
        //"against",
        //"all",
        "am",
        "an",
        "any",
        "are",
        "as",
        "at",
        "be",
        "because",
        "been",
        //"before",
        "being",
        //"below",
        //"between",
        //"both",
        //"but",
        "by",
        //"can",
        //"could",
        "did",
        "do",
        "does",
        "doing",
        "do",
        //"down",
        "during",
        //"each",
        "for",
        "from",
        //"further",
        "had",
        "has",
        "have",
        "having",
        "how",
        "'ll",
        "'m",
        "if",
        "in",
        "into",
        "is",
        "let",
        //"more",
        //"most",
        //"must",
        "of",
        //"off",
        "on",
        //"once",
        //"only",
        //"or",
        //"ought",
        "out",
        "over",
        //"same",
        "'d",
        //"should",
        //"so",
        //"some",
        "such",
        "than",
        "that",
        "the",
        "then",
        "'re",
        "through",
        "to",
        //"too",
        "under",
        //"until",
        "up",
        //"very",
        "was",
        "were",
        "what",
        "when",
        "where",
        "which",
        "while",
        "whom",
        "why",
        "with"
        //"would"
        )
    
    val badStopwords = Set( // these stop words can change the meaning
        "about",
        "above",
        "after",
        "again",
        "against",
        "all",
        "before",
        "below",
        "between",
        "both",
        "but",
        "can",
        "could",
        "down",
        "each",
        "further",
        "more",
        "most",
        "must",
        "n't",
        "no",
        "not",
        "off",
        "once",
        "only",
        "or",
        "ought",
        "same",
        "should",
        "so",
        "some",
        "too",
        "until",
        "very",
        "would"
    )

    val stopwordsSmall = Set(
        "",
        ",",
        "'s",
        "a",
        "about",
        "above",
        "after",
        "again",
        "against",
        "all",
        "am",
        "an",
        "any",
        "are",
        "as",
        "at",
        "be",
        "because",
        "been",
        "before",
        "being",
        "below",
        "between",
        "both",
        "but",
        "by",
        "can",
        "could",
        "did",
        "do",
        "does",
        "doing",
        "do",
        "down",
        "during",
        "each",
        "for",
        "from",
        "further",
        "had",
        "has",
        "have",
        "having",
        "how",
        "'ll",
        "'m",
        "if",
        "in",
        "into",
        "is",
        "let",
        "more",
        "most",
        "must",
        "of",
        "off",
        "on",
        "once",
        "only",
        "or",
        "ought",
        "out",
        "over",
        "same",
        "'d",
        "should",
        "so",
        "some",
        "such",
        "than",
        "that",
        "the",
        "then",
        "'re",
        "through",
        "to",
        "too",
        "under",
        "until",
        "up",
        "very",
        "was",
        "were",
        "what",
        "when",
        "where",
        "which",
        "while",
        "whom",
        "why",
        "with",
        "would")

    val stopwords = Set(
        "",
        ",",
        "'s",
        "a",
        "about",
        "above",
        "after",
        "again",
        "against",
        "all",
        "am",
        "an",
        //"and",
        "any",
        "are",
        "as",
        "at",
        "be",
        "because",
        "been",
        "before",
        "being",
        "below",
        "between",
        "both",
        "but",
        "by",
        "can",
        "could",
        "did",
        "do",
        "does",
        "doing",
        "do",
        "down",
        "during",
        "each",
        "few",
        "for",
        "from",
        "further",
        "had",
        "has",
        "have",
        "having",
        "he",
        "her",
        "here",
        "hers",
        "herself",
        "him",
        "himself",
        "his",
        "how",
        "i",
        "'ll",
        "'m",
        "'ve",
        "if",
        "in",
        "into",
        "is",
        "it",
        "its",
        "itself",
        "let",
        "me",
        "more",
        "most",
        "must",
        "my",
        "myself",
        //"no",
        "nor",
        //"not",
        "of",
        "off",
        "on",
        "once",
        "only",
        "or",
        "other",
        "ought",
        "our",
        "ours",
        "ourselves",
        "out",
        "over",
        "own",
        "same",
        "she",
        "'d",
        "should",
        "so",
        "some",
        "such",
        "than",
        "that",
        "the",
        "their",
        "theirs",
        "them",
        "themselves",
        "then",
        "there",
        "these",
        "they",
        "'re",
        "this",
        "those",
        "through",
        "to",
        "too",
        "under",
        "until",
        "up",
        "very",
        "was",
        "we",
        "were",
        "what",
        "when",
        "where",
        "which",
        "while",
        "who",
        "whom",
        "why",
        "with",
        "would",
        "you",
        "your",
        "yours",
        "yourself",
        "yourselves")
}

