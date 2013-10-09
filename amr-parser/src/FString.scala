package edu.cmu.lti.nlp.amr

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
        new FString(fstring.list ::: list)
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
}

