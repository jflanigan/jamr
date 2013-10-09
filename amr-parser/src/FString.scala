package edu.cmu.lti.nlp.amr

import scala.io.Source
import scala.util.matching.Regex
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer

class FString private (private val list: List[Int]) {   // private constructor and private member list
    private def this(str: String) = this(if (FString.lookup.contains(str)) {    // create an FString by doing FString("string")
        List(FString.lookup(str))
    } else {
        FString.strings += str
        FString.lookup += (str -> (FString.strings.size-1))
        List(FString.strings.size-1)
    })
    def + (fstring: FString) : FString = { // it is effecient to do fstr1+fstr2+fstr3 but not fstr1+(fstr2+fstr3)
        new FString(fstring.list ::: this.list)
    }
    override def toString() : String = {
        "FString("+mkString(":")+")"
    }
    def mkString(sep: String = "") : String = {
        list.view.reverse.map(x => FString.strings(x)).mkString(sep)
    }
}

object FString {
    val strings: ArrayBuffer[String] = new ArrayBuffer()
    val lookup: Map[String, Int] = Map.empty[String, Int]
    def apply(str: String) : FString = {    // Factory method for creating an FString
        new FString(str)
    }

/***************** Demo *******************/
    def main(args: Array[String]) {
        if(args.size != 1) {
            println("usage: scala -classpath . edu.cmu.lti.nlp.amr.FString ../aligner/snt")
            sys.exit(1)
        }

        val corpus : Array[String] = for {line <- Source.fromFile(args(0)).getLines.toArray
             i <- line.split(" +")} yield i

        { // Test 1 (fast)
        println("Starting test 1")
//        val tokens = ArrayBuffer.empty[FString]
        val unigrams = Map.empty[FString, Double]
        val bigrams = Map.empty[FString, Double]
        val trigrams = Map.empty[FString, Double]

        var prev1, prev2 = FString("")
        for (i <- corpus) {
            val token = FString(i)
            unigrams += (token -> (unigrams.getOrElse(token, 0.0)+1))
            bigrams += ((token + prev1) -> (bigrams.getOrElse(token + prev1, 0.0)+1))
            trigrams += ((token + prev1 + prev2) -> (trigrams.getOrElse(token + prev1 + prev2, 0.0)+1))
            prev2 = prev1
            prev1 = token
        }
        println("Tokens: " + corpus.size.toString)
        println("Unigrams: " + unigrams.size.toString)
        println("Bigrams: " + bigrams.size.toString)
        println("Trigrams: " + trigrams.size.toString)
        }

    }
}

