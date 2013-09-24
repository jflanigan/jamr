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
        var recordNumber = 0

        var words = corpus(recordNumber).sentence
        var amr = corpus(recordNumber).graph.root.prettyString(detail = 1, pretty = true).split("\n")

        val wordList = new ListView(words)
        val amrList = new ListView(amr)
        val nextButton = new Button { text = "Next" }
        val curLabel = new Label { text = recordNumber.toString }
        //val sentenceLabel = new Label { text = words.mkString(" ") }
        val prevButton = new Button { text = "Prev" }
        contents = new BoxPanel(Orientation.Vertical) {
            //contents += sentenceLabel
            contents += new BoxPanel(Orientation.Horizontal) {
                contents += new ScrollPane(wordList)
                contents += new ScrollPane(amrList)
            }
            contents += new BoxPanel(Orientation.Horizontal) {
                contents += prevButton
                contents += curLabel
                contents += nextButton
            }
        }
        listenTo(nextButton)
        listenTo(prevButton)
        reactions += {
            case ButtonClicked(b) =>
                if (b == nextButton) {
                    recordNumber += 1
                    updateView
                } else if (b == prevButton) {
                    recordNumber -= 1
                    updateView
                }
        }
        
        def updateView() {
            words = corpus(recordNumber).sentence
            amr = corpus(recordNumber).graph.root.prettyString(detail = 1, pretty = true).split("\n")
            curLabel.text = recordNumber.toString
            //sentenceLabel.text = "<html>"+words.mkString(" ")+"</html>"
            wordList.listData = words
            amrList.listData = amr
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
                if block.matches("(.|\n)*\n\\((.|\n)*")     // needs to contain some AMR
            } yield toAMRTriple(block)
        )

        super.main(args)    // Start GUI
    }
}

