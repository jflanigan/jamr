package edu.cmu.lti.nlp.amr

import java.awt.FlowLayout
import java.awt.Color
import java.awt.Font
import javax.swing.JFrame
import javax.swing.JList
import javax.swing.JOptionPane
import javax.swing.JScrollPane
import javax.swing.event.ListSelectionListener
import javax.swing.event.ListSelectionEvent
import javax.swing.ListSelectionModel
import javax.swing.DefaultListCellRenderer

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

    val colors = Array(Color.RED, Color.GREEN, Color.MAGENTA, Color.ORANGE, Color.PINK, Color.CYAN, Color.BLUE )

    var corpus = LazyArray(Iterator[AMRTriple]())

    def top = new MainFrame {
        /*---------------------- Initialization --------------------*/
        title = "AMR AlignerTool v.1a"
        var recordNumber = 0
        var annotationIndex = 0

        var words = corpus(recordNumber).sentence
        var graph = corpus(recordNumber).graph
        graph.loadSpans(corpus(recordNumber).spans(annotationIndex), words)
        var amr = graph.root.prettyString(detail = 2, pretty = true).split("\n")
        val ID = """.*\[([^\]]+)\].*""".r
        var ids = graph.root.prettyString(detail = 2, pretty = true).split("\n").map(x => {val ID(id) = x; id})
        var wordIndexToSpan = SpanLoader.toWordMap(graph.spans, words)
        var spans = for {(span, i) <- graph.spans.zipWithIndex
            } yield "Span "+(i+1).toString+": "+span.start+"-"+span.end+"  "+span.words+" => "+span.amr

        val wordList = new ListView(words)
        val amrList = new ListView(amr)
        val spanList = new ListView(spans)

        /*---------------------- Color Renderers -------------------*/
        amrList.renderer = ListView.Renderer.wrap(new DefaultListCellRenderer() {
            override def getListCellRendererComponent(list: JList, value: Object, index: Int, isSelected: Boolean, cellHasFocus: Boolean) : java.awt.Component = {
                val spanIndex = graph.getNodeById(ids(index)).span
                if (isSelected) {
                    setBackground(list.getSelectionBackground)
                    if (spanIndex == None) {
                        setForeground(list.getSelectionForeground)
                    } else {
                        val Some(i) = spanIndex
                        setForeground(colors(i%colors.size))
                    }
                } else {
                    setBackground(list.getBackground)
                    if (spanIndex == None) {
                        setForeground(list.getForeground)
                    } else {
                        val Some(i) = spanIndex
                        setForeground(colors(i%colors.size))
                    }
                }
                setText(amr(index))
                //setText(value.asInstanceOf[String])
                setFont(list.getFont)
                return this.asInstanceOf[java.awt.Component]
            }
        })
        wordList.renderer = ListView.Renderer.wrap(new DefaultListCellRenderer() {
            override def getListCellRendererComponent(list: JList, value: Object, index: Int, isSelected: Boolean, cellHasFocus: Boolean) : java.awt.Component = {
                val spanIndex = wordIndexToSpan(index)
                if (isSelected) {
                    setBackground(list.getSelectionBackground)
                    if (spanIndex == None) {
                        setForeground(list.getSelectionForeground)
                    } else {
                        val Some(i) = spanIndex
                        setForeground(colors(i%colors.size))
                    }
                } else {
                    setBackground(list.getBackground)
                    if (spanIndex == None) {
                        setForeground(list.getForeground)
                    } else {
                        val Some(i) = spanIndex
                        setForeground(colors(i%colors.size))
                    }
                }
                setText(words(index))
                //setText(value.asInstanceOf[String])
                setFont(list.getFont)
                return this.asInstanceOf[java.awt.Component]
            }
        })
        /*spanList.renderer = ListView.Renderer.wrap(new DefaultListCellRenderer() {
            override def getListCellRendererComponent(list: JList, value: Object, index: Int, isSelected: Boolean, cellHasFocus: Boolean) : java.awt.Component = {
                val spanIndex = Some(index)
                if (isSelected) {
                    setBackground(list.getSelectionBackground)
                    if (spanIndex == None) {
                        setForeground(list.getSelectionForeground)
                    } else {
                        val Some(i) = spanIndex
                        setForeground(colors(i%colors.size))
                    }
                } else {
                    setBackground(list.getBackground)
                    if (spanIndex == None) {
                        setForeground(list.getForeground)
                    } else {
                        val Some(i) = spanIndex
                        setForeground(colors(i%colors.size))
                    }
                }
                setText(spans(index))
                setFont(list.getFont)
                return this.asInstanceOf[java.awt.Component]
            }
        })*/

        /*------------------------- Layout --------------------------*/
        val nextButton = new Button { text = "Next" }
        val curLabel = new Label { text = recordNumber.toString }
        val prevButton = new Button { text = "Prev" }
        contents = new BoxPanel(Orientation.Vertical) {
            contents += new BoxPanel(Orientation.Vertical) {
                contents += new BoxPanel(Orientation.Horizontal) {
                    contents += new ScrollPane(wordList)
                    contents += new ScrollPane(amrList)
                }
                contents += spanList
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

        /*------------------------ Update View ---------------------*/
        def updateView() {
/*            words = corpus(recordNumber).sentence
            graph = corpus(recordNumber).graph
            amr = graph.root.prettyString(detail = 1, pretty = true).split("\n")
            ids = graph.root.prettyString(detail = 2, pretty = true).split("\n").map(x => {val ID(id) = x; id})
            wordIndexToSpan = Span.toWordMap(corpus(recordNumber).spans(0), words) */

            words = corpus(recordNumber).sentence
            graph = corpus(recordNumber).graph
            graph.loadSpans(corpus(recordNumber).spans(annotationIndex), words)
            amr = graph.root.prettyString(detail = 2, pretty = true).split("\n")
            ids = graph.root.prettyString(detail = 2, pretty = true).split("\n").map(x => {val ID(id) = x; id})
            wordIndexToSpan = SpanLoader.toWordMap(graph.spans, words)
            spans = for {(span, i) <- graph.spans.zipWithIndex
                } yield "Span "+(i+1).toString+": "+span.start+"-"+span.end+"  "+span.words+" => "+span.amr

            curLabel.text = recordNumber.toString
            wordList.listData = words
            amrList.listData = amr
            spanList.listData = spans

            for ((span, i) <- graph.spans.zipWithIndex) {
                println(spans)
            }
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

