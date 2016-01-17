package edu.cmu.lti.nlp.amr

import java.util.Properties
import edu.stanford.nlp.pipeline.StanfordCoreNLP
import edu.stanford.nlp.trees._
import edu.stanford.nlp.util.Filters
import scala.collection.JavaConversions._
import edu.stanford.nlp.ling.CoreAnnotations._
import edu.stanford.nlp.trees.TreeCoreAnnotations.TreeAnnotation


case class ConllToken(index: Option[Int],
                      form: Option[String],
                      lemma: Option[String],
                      pos: Option[String],
                      cpos: Option[String],
                      feats: Option[String],
                      gov: Option[Int],
                      deprel: Option[String],
                      phead: Option[Int],
                      pdeprel: Option[String]) extends Iterable[Option[_]] {

  override def toString() = {
    map(_.getOrElse("_")).mkString("\t")
  }

  override def iterator = productIterator.asInstanceOf[Iterator[Option[_]]]
}

/**
 * Thin wrapper around edu.stanford.nlp.pipeline.StanfordCoreNLP
 *
 * @author sthomson@cs.cmu.edu
 */
class StanfordProcessor {
  private val processor = {
    val props = new Properties()
    props.setProperty("annotators", "tokenize,ssplit,parse")
    props.setProperty("parse.model", "edu/stanford/nlp/models/lexparser/englishRNN.ser.gz")
    new StanfordCoreNLP(props)
  }

  // don't filter out punctuation dependencies
  private val grammaticalStructureFactory = new EnglishGrammaticalStructureFactory(Filters.acceptFilter())

  /**
   * Parse to basic dependencies in conllx format.
   * Undoes any changes Stanford makes to the word forms.
   */
  def parse(input: String): List[List[ConllToken]] = {
    val annotation = processor.process(input)
    val sentences = annotation.get(classOf[SentencesAnnotation]).toList
    for (sentence <- sentences) yield {
      val tree = sentence.get(classOf[TreeAnnotation])
      val gs = grammaticalStructureFactory.newGrammaticalStructure(tree)
      val deps = gs.typedDependencies().toList sortBy (_.dep.index)
      val tokens = sentence.get(classOf[TokensAnnotation]).toList
      for ((token, dep) <- tokens zip deps) yield {
        val start = token.get(classOf[CharacterOffsetBeginAnnotation])
        val end = token.get(classOf[CharacterOffsetEndAnnotation])
        val pos = token.get(classOf[PartOfSpeechAnnotation])
        ConllToken(
          Some(dep.dep.index),
          Some(input.substring(start, end)),
          None,
          Some(pos),
          Some(pos),
          None,
          Some(dep.gov.index),
          Some(dep.reln.getShortName),
          None,
          None
        )
      }
    }
  }

  def parseToConll(input: String) = parse(input).map(_.mkString("\n")).mkString("\n\n")
}

object RunStanfordParser extends App {
  val processor = new StanfordProcessor

  for (sentence <- Source.stdin.getLines()) {
    println(processor.parseToConll(sentence).replaceAllLiterally("\n\n","\n") + "\n")
  }
}
