package edu.cmu.lti.nlp.amr

import scala.swing._
import scala.swing.event._

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

import Corpus._

object AlignerTool extends SimpleSwingApplication {
    val usage = """Usage: scala -classpath . edu.cmu.lti.nlp.amr.AlignerTool filename"""
    type OptionMap = Map[Symbol, Any]

    var corpus = LazyArray(Iterator[AMRTriple]())

    def top = new MainFrame {
        title = "AMR AlignerTool v.1a"
        val colors = List("Black", " Blue", "  Cyan", "    Dark Gray", "Gray", "Green", "Light Gray", "Magenta", "Orange", "Pink", "Red", "White", "Yellow")

        val amr = corpus(0).graph.root.prettyString(detail = 2, pretty = true).split("\n")

        val list = new ListView(amr) {
            
        }
        contents = new BoxPanel(Orientation.Vertical) {
            contents += list
            border = Swing.EmptyBorder(30,30,10,30)
        }
    }

    def parseOptions(map : OptionMap, list: List[String]) : OptionMap = {
        def isSwitch(s : String) = (s(0) == '-')
        list match {
            case Nil => map
            case "-v" :: value :: tail =>
                      parseOptions(map ++ Map('verbosity -> value.toInt), tail)
            case string :: opt2 :: tail if isSwitch(opt2) => 
                      parseOptions(map ++ Map('infile -> string), list.tail)
            case string :: Nil =>  parseOptions(map ++ Map('infile -> string), list.tail)
            case option :: tail => println("Error: Unknown option "+option)
                               sys.exit(1)
      }
    }

    override def main(args: Array[String]) {

        if (args.length == 0) { println(usage); sys.exit(1) }

        val options = parseOptions(Map(),args.toList)
        if (options.contains('verbosity)) {
            verbosity = options('verbosity).asInstanceOf[Int]
        }
        if (!options.contains('infile)) {
            System.err.println("Error: No AMR file specified")
            sys.exit(1)
        }

        val filename = options('infile).asInstanceOf[String]

        corpus = LazyArray(
            for {
                block <- splitOnNewline(Source.fromFile(filename).getLines)
                //if block.matches(""".*\n[ ]*\(""") // It needs to contain some AMR
            } yield toAMRTriple(block)
        )

        for (i <- corpus) {
            println(i)
        }

        super.main(args)    // Start GUI
    }
}

