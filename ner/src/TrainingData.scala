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

/********************************** Training Data *********************************/

case class Token(token: String, pos: String, tag: Int)

class TrainingData(filename: String) {
    val tagId : Map[String, Int] = Map[String, Int]()
    private val taglist : ArrayBuffer[String] = ArrayBuffer.empty[String]
    val corpus : List[Sentence] = load(filename)

    def tagset() : Array[String] = taglist.toArray
    private def addTag(tag: String) = {
        if (!tagId.contains(tag)) {
            taglist += tag
            tagId += (tag -> taglist.size)
        }
    }

    private class ConllParser extends JavaTokenParsers {
        def field : Parser[String] = """[^ \t][^ \t]*""".r
        def token : Parser[Token] = field~field~field~field ^^ { case token~pos~chunk~tag => { addTag(tag); Token(token, pos, tagId(tag)) } }
        def sentence : Parser[Sentence] = rep(token) ^^ toarray
        def toarray(l : List[Token]) : Array[Token] = { l.toArray } // otherwise doesn't compile
    }

    private def load(filename: String) : List[Sentence] = {
        addTag("<START>")
        val parser = new ConllParser()
        val iterator = Source.fromFile(filename).getLines()
        val corpus = iterator.map(parser.parseAll(parser.sentence, _) match { case parser.Success(e, _) => e}).toList
        addTag("<STOP>")
        return corpus
    }
}

//object TrainingData {
//    type Sentence = Array[Token]     // A sentence is just a list of tokens
//}



