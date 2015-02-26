package edu.cmu.lti.nlp.amr
import edu.cmu.lti.nlp.amr._

import scala.util.matching.Regex
import scala.collection.mutable.{Map, Set, ArrayBuffer}

package object Generate {
    def Label(label: String) : String = {
        return label.drop(1).toUpperCase.replaceAll("-","_")
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
        return splitStr(unEscape(str, esc), "\t")
    }
    def projectPos(posAnno: Annotation[Array[String]]) : Array[String] = {
        val sentence : Array[String] = posAnno.snt // tokenized sentence
        val pos = (0 until sentence.size).map(i => {
            val span = posAnno.annotationSpan(i,i+1)
            val posList = posAnno.annotation.slice(span._1, span._2)    // should always return a non-empty array
            posList.last    // take the last one (works well in English)
        }).toArray
        return pos
    }
    def splitStr(str: String, sep: String) : Array[String] = {
        // String.split doesn't work the way you would think.  Here is a better version.
        // See https://issues.scala-lang.org/browse/SI-5069
        splitStrToList(str, sep).toArray
    }
    def splitStrToList(str: String, sep: String) : List[String] = {
        val i = str.indexOfSlice(sep)
        if(i == -1) {
            List(str)
        } else {
            val (str1, str2) = str.splitAt(i)
            val rest = str2.drop(sep.size)
            str1 :: splitStrToList(rest, sep)
        }
    }
    val stopwords = Set(
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
        "and",
        "any",
        "are",
        "aren't",
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
        "can't",
        "cannot",
        "could",
        "couldn't",
        "did",
        "didn't",
        "do",
        "does",
        "doesn't",
        "doing",
        "don't",
        "down",
        "during",
        "each",
        "few",
        "for",
        "from",
        "further",
        "had",
        "hadn't",
        "has",
        "hasn't",
        "have",
        "haven't",
        "having",
        "he",
        "he'd",
        "he'll",
        "he's",
        "her",
        "here",
        "here's",
        "hers",
        "herself",
        "him",
        "himself",
        "his",
        "how",
        "how's",
        "i",
        "i'd",
        "i'll",
        "i'm",
        "i've",
        "if",
        "in",
        "into",
        "is",
        "isn't",
        "it",
        "it's",
        "its",
        "itself",
        "let's",
        "me",
        "more",
        "most",
        "mustn't",
        "my",
        "myself",
        "no",
        "nor",
        "not",
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
        "shan't",
        "she",
        "she'd",
        "she'll",
        "she's",
        "should",
        "shouldn't",
        "so",
        "some",
        "such",
        "than",
        "that",
        "that's",
        "the",
        "their",
        "theirs",
        "them",
        "themselves",
        "then",
        "there",
        "there's",
        "these",
        "they",
        "they'd",
        "they'll",
        "they're",
        "they've",
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
        "wasn't",
        "we",
        "we'd",
        "we'll",
        "we're",
        "we've",
        "were",
        "weren't",
        "what",
        "what's",
        "when",
        "when's",
        "where",
        "where's",
        "which",
        "while",
        "who",
        "who's",
        "whom",
        "why",
        "why's",
        "with",
        "won't",
        "would",
        "wouldn't",
        "you",
        "you'd",
        "you'll",
        "you're",
        "you've",
        "your",
        "yours",
        "yourself",
        "yourselves")
}

