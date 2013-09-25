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
        var amr = graph.root.prettyString(detail = 1, pretty = true).split("\n")
        val ID = """.*\[([^\]]+)\].*""".r
        var ids = graph.root.prettyString(detail = 2, pretty = true).split("\n").map(x => {val ID(id) = x; id})
        var wordIndexToSpan = SpanLoader.toWordMap(graph.spans, words)
        var spans = for {(span, i) <- graph.spans.zipWithIndex
            } yield "Span "+(i+1).toString+": "+span.start+"-"+span.end+"  "+span.words+" => "+span.amr
        var spanToAMRIndex : ArrayBuffer[Set[Int]] = graph.spans.map(x => Set()++x.nodeIds.map(ids.indexOf(_)))
        def spanToWordIndex(i: Int) : Seq[Int] = {
            Range(graph.spans(i).start, graph.spans(i).end)
        }

        val wordList = new ListView(words)
        val amrList = new ListView(amr)
        val spanList = new ListView(spans)
        spanList.selection.intervalMode = ListView.IntervalMode.Single

        var spanSelection = -1  // variable the keeps track of which span # is currently highlighted (across all views)

        /*---------------------- Color Renderers -------------------*/
        amrList.renderer = ListView.Renderer.wrap(new DefaultListCellRenderer() {
            override def getListCellRendererComponent(list: JList, value: Object, index: Int, isSelected: Boolean, cellHasFocus: Boolean) : java.awt.Component = {
                val spanIndex = graph.getNodeById(ids(index)).span
                if (!dynamicSelect) {
                    if(isSelected) {
                        setBackground(list.getSelectionBackground)
                        spanIndex match {
                            case None => setForeground(list.getSelectionForeground)
                            case Some(i) => setForeground(colors(i%colors.size))
                        }
                    } else {
                        setBackground(list.getBackground)
                        spanIndex match {
                            case None => setForeground(list.getForeground)
                            case Some(i) => setForeground(colors(i%colors.size))
                        }
                    }
                } else {
                if (cellHasFocus) {
                    setBackground(list.getSelectionBackground)
                    if (spanIndex == None) {
                        setForeground(list.getSelectionForeground)
                        if (!keypressed && spanSelection != -1) {
                            spanSelection = -1
                            amrList.repaint
                            wordList.repaint
                        }
                    } else {
                        val Some(i) = spanIndex
                        setForeground(colors(i%colors.size))
                        if (!keypressed && spanSelection != i) {
                            spanSelection = i
                            amrList.repaint     // if changed, repaint
                            wordList.repaint
                        }
                    }
                } else {
                    if (spanIndex == None) {
                        setBackground(Color.RED)
                        setForeground(list.getForeground)
                    } else {
                        val Some(i) = spanIndex
                        if (spanSelection == i && spanToAMRIndex(i).contains(index)) {
                            setBackground(list.getSelectionBackground)
                            setForeground(colors(i%colors.size))
                        } else {
                            setBackground(list.getBackground)
                            setForeground(colors(i%colors.size))
                        }
                    }
                }
                }
                setText(amr(index))
                setFont(list.getFont)
                return this.asInstanceOf[java.awt.Component]
            }
        })
        wordList.renderer = ListView.Renderer.wrap(new DefaultListCellRenderer() {
            override def getListCellRendererComponent(list: JList, value: Object, index: Int, isSelected: Boolean, cellHasFocus: Boolean) : java.awt.Component = {
                val spanIndex = wordIndexToSpan(index)
                if (!dynamicSelect) {
                    if(isSelected) {
                        setBackground(list.getSelectionBackground)
                        spanIndex.size match {
                            case 0 => setForeground(list.getSelectionForeground)
                            case _ => setForeground(colors(spanIndex(0)%colors.size))
                        }
                    } else {
                        setBackground(list.getBackground)
                        spanIndex.size match {
                            case 0 => setForeground(list.getForeground)
                            case 1 => setForeground(colors(spanIndex(0)%colors.size))
                            case _ => setForeground(colors(spanIndex(0)%colors.size))
                                      setBackground(Color.RED)
                        }
                    }
                } else {
                if (cellHasFocus) {
                    setBackground(list.getSelectionBackground)
                    if (spanIndex.size == 0) {
                        setForeground(list.getSelectionForeground)
                        if (!keypressed && spanSelection != -1) {
                            spanSelection = -1
                            amrList.repaint
                            wordList.repaint
                        }
                     } else {
                        val i = spanIndex(0)
                        setForeground(colors(i%colors.size))
                        if (!keypressed && spanSelection != i) {
                            spanSelection = i
                            amrList.repaint
                            wordList.repaint
                        }
                    }
                } else {
                    if (spanIndex.size == 0) {
                        setBackground(list.getBackground)
                        setForeground(list.getForeground)
                    } else {
                        val i = spanIndex(0)
                        if (spanSelection == i) {
                            setBackground(list.getSelectionBackground)
                            setForeground(colors(i%colors.size))
                        } else{
                            setForeground(colors(i%colors.size))
                            if (spanIndex.size == 1) {
                                setBackground(list.getBackground)
                            } else {
                                setBackground(Color.RED)
                            }
                        }
                    }
                }
                }
                setText(words(index))
                setFont(list.getFont)
                return this.asInstanceOf[java.awt.Component]
            }
        })

        /*------------------------- Layout --------------------------*/
        val nextButton = new Button { text = "Next" }
        val curLabel = new Label { text = recordNumber.toString }
        val prevButton = new Button { text = "Prev" }
        contents = new BoxPanel(Orientation.Vertical) {
            contents += new BoxPanel(Orientation.Horizontal) {
                contents += prevButton
                contents += curLabel
                contents += nextButton
            }
            contents += new BoxPanel(Orientation.Vertical) {
                contents += spanList
                contents += new BoxPanel(Orientation.Horizontal) {
                    contents += new ScrollPane(wordList)
                    contents += new ScrollPane(amrList)
                }
            }
        }
        /*------------------------- Listeners --------------------------*/
        listenTo(nextButton)
        listenTo(prevButton)
        reactions += {
            case ButtonClicked(this.nextButton) =>
                recordNumber += 1
                updateView
            case ButtonClicked(this.prevButton) =>
                recordNumber -= 1
                updateView
        }

        listenTo(spanList.selection)
        reactions += {
            case SelectionChanged(this.spanList) if !spanList.selection.adjusting =>
                val indices = spanList.selection.indices
                if (indices.size > 0) {
                    val i = indices.toList(0)  // indices will be of size one
                    spanSelection = i
                    listIgnore = Array(true, true)
                    listSelection(0) = spanToAMRIndex(i)
                    listSelection(1) = Set()++spanToWordIndex(i)
                    amrList.selectIndices(spanToAMRIndex(i).toSeq :_* )
                    wordList.selectIndices(spanToWordIndex(i) :_* )
                }
        }

        var keypressed = false
        listenTo(amrList.keys)
        reactions += {
            case KeyPressed(_, Key.Shift, _, _) =>
                keypressed = true
                println("Shift pressed")
            case KeyReleased(_, Key.Shift, _, _) =>
                keypressed = false
                println("Shift release")
            case KeyPressed(_, Key.Control, _, _) =>
                keypressed = true
                println("Control pressed")
            case KeyReleased(_, Key.Control, _, _) =>
                keypressed = false
                println("Control release")
         }

        val lists = Array(amrList, wordList)
        var listIgnore = Array(false, false)
        var listSelection = Array(Set(-1), Set(-1))
        for (i <- Range(0, 2)) {
            val list = lists(i)
            listenTo(lists(i).selection)
            reactions += {
            case SelectionChanged(`list`) if !lists(i).selection.adjusting =>
                val indices = lists(i).selection.indices
                logger(1,"Indices = "+indices.toString)
                logger(1,"Selection = "+listSelection(i))
                logger(1,"Ignore = "+listIgnore(i).toString)
                if (!keypressed) {
                    if (!listIgnore(i)) {
                        if (indices.size == 1) {
                            //val nodeIndex : Int = indices.toList(0)
                            var spanIndex : Option[Int] = None
                            if (i == 0) {
                                val nodeIndex : Int = indices.toList(0)
                                spanIndex = graph.getNodeById(ids(nodeIndex)).span
                            } else {
                                val wordIndex : Int = indices.toList(0)
                                val spanIndexArray = wordIndexToSpan(wordIndex)
                                if (spanIndexArray.size > 0) {
                                    spanIndex = Some(spanIndexArray(0)) // Take the first element
                                }   // otherwise it leaves spanIndex as None
                            }
                            if (spanIndex != None) {
                                val Some(j) = spanIndex
                                logger(1,"Setting to "+spanToAMRIndex(j))
                                spanSelection = j
                                listIgnore = Array(true, true)
                                listSelection(0) = spanToAMRIndex(j)
                                amrList.selectIndices(spanToAMRIndex(j).toSeq :_* )
                                listSelection(1) = Set()++spanToWordIndex(j)
                                wordList.selectIndices(spanToWordIndex(j) :_* )
                                logger(1,"Clearing spanList")
                                spanList.selectIndices()
                             } else {
                                if (lists((i+1)%2).selection.indices != Set()) {
                                    logger(1,"Clearing list = "+((i+1)%2).toString)
                                    spanSelection = -1
                                    listSelection((i+1)%2) = Set.empty[Int]
                                    listIgnore((i+1)%2) = true
                                    lists((i+1)%2).selectIndices()
                                }
                                logger(1,"Setting mine to "+indices)
                                listSelection(i) = indices
                                logger(1,"Clearing spanList")
                                spanList.selectIndices()
                            }
                        }
                    } else {
                        if (listSelection(i) == indices) {
                            listIgnore(i) = false
                        }
                    }
                } else {
                    // TODO: update the span
                    //listSelection(i) = indices
                }
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
            amr = graph.root.prettyString(detail = 1, pretty = true).split("\n")
            ids = graph.root.prettyString(detail = 2, pretty = true).split("\n").map(x => {val ID(id) = x; id})
            wordIndexToSpan = SpanLoader.toWordMap(graph.spans, words)
            spans = for {(span, i) <- graph.spans.zipWithIndex
                } yield "Span "+(i+1).toString+": "+span.start+"-"+span.end+"  "+span.words+" => "+span.amr
            spanToAMRIndex = graph.spans.map(x => Set()++x.nodeIds.map(ids.indexOf(_))) 

            curLabel.text = recordNumber.toString
            wordList.listData = words
            amrList.listData = amr
            spanList.listData = spans

            spanSelection = -1
            listIgnore = Array(false, false)
            listSelection = Array(Set(-1), Set(-1))
 
            for ((span, i) <- graph.spans.zipWithIndex) {
                println(spans)
            }
        }
    }

    def parseOptions(map : OptionMap, list: List[String]) : OptionMap = {
        def isSwitch(s : String) = (s(0) == '-')
        list match {
            case Nil => map
            case "--refresh" :: tail =>
                      parseOptions(map ++ Map('refresh -> true), tail)
            case "-v" :: value :: tail =>
                      parseOptions(map ++ Map('verbosity -> value.toInt), tail)
            case string :: opt2 :: tail if isSwitch(opt2) => 
                      parseOptions(map ++ Map('infile -> string), list.tail)
            case string :: Nil =>  parseOptions(map ++ Map('infile -> string), list.tail)
            case option :: tail => println("Error: Unknown option "+option)
                               sys.exit(1)
      }
    }

    var dynamicSelect = false

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
        dynamicSelect = options.contains('refresh)

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

