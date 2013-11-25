package edu.cs.umass

import cc.factorie.app.nlp.segment._

import scala.util.control.Breaks._
import java.io._

import scala.reflect.io.Path
import scala.io.Source
import cc.factorie.app.nlp._
import scala.collection.mutable.ArrayBuffer


class HyphenConcatenator(dictionary: Set[String] = Set.empty[String], useTokens: Boolean) extends DocumentAnnotator {

  def process(document: Document) = {

    lazy val dictionaryFromDocWords = buildDictionaryFromDocWords(document.tokens)

    def eligibleForMerge(first: String, last:String) = dictionary((first+last).toLowerCase) || (useTokens && dictionaryFromDocWords((first+last).toLowerCase))

    var wasLastOperationMerge = false

    for(section <- document.sections){
      val buffer = new ArrayBuffer[Token]()
      val slidingIterator = section.tokens.sliding(3)
      while(slidingIterator.hasNext){
        slidingIterator.next() match {
          case tokens if tokens.size == 3 && tokens(1).string=="-" &&
            tokens(2).hasFollowingWhitespace && eligibleForMerge(tokens(0).string, tokens(2).string) =>
            val first = tokens.head
            val last  = tokens.last
            //create a new token and set it's string offset to the first to the last token
            val t = new Token(first.stringStart, last.stringEnd)
            //add a TokenString attr to output the concatenated string
            t.attr += new TokenString(t, first.string+last.string)
            buffer += t
            slidingIterator.drop(2)
            wasLastOperationMerge = true
          case tokens =>
            buffer += tokens.head
            wasLastOperationMerge = false
        }
      }
      //if last 3 tokens did not qualify for a merge, the last 2 tokens were skipped because of the sliding window of 3
      //add them now
      if(!wasLastOperationMerge) buffer ++= section.tokens.takeRight(2)
      while(section.tokens.size>0){
        section.remove(0)
      }
      section ++= buffer
    }
    document
  }

  def buildDictionaryFromDocWords(tokens: Iterable[Token]) = tokens.filterNot(_.isPunctuation).map(_.string).toSet

  // NOTE: this method may mutate and return the same document that was passed in
  def prereqAttrs = List(classOf[Token])

  def postAttrs = List(classOf[Token])

  /** How the annotation of this DocumentAnnotator should be printed in one-word-per-line (OWPL) format.
      If there is no per-token annotation, return null.  Used in Document.owplString. */
  def tokenAnnotationString(token: Token) = token.stringStart.toString+'\t'+token.stringEnd.toString
}


object HyphenConcatenator extends App {
  val d = new Document("How the annotation of this DocumentAnnotator should be printed annota-tion in one-word-per-line (OWPL) format. ")
  d.annotators(classOf[Token]) = UnknownDocumentAnnotator.getClass
  val b = new DeterministicTokenizer()
  val doc = b.process(d)
  doc.tokens.foreach(t=>print(t.string+" "))
  val doc2 = new HyphenConcatenator(useTokens = true).process(doc)
  println(doc2.tokenCount)
  doc2.tokens.foreach(t=>print(t.string+" "))
}


object Rexa_Data_Cleanup {
  def main(args:Array[String]){
  val inputdocuments = "/iesl/canvas/lvnair/rexa_corpus/rexa_corpus_fullpapers.txt"
  //val inputdocuments = "/Users/lvnair/Documents/code/Kmeans/rexa_test"
  //val inputdocuments = "/iesl/canvas/lvnair/rexa_corpus/rexa_corpus_fullpapers.txt"
  //val removehyphendocuments =  "/Users/lvnair/Documents/code/Kmeans/rexa_test_hyphenclean"
  val source = scala.io.Source.fromFile(new File(inputdocuments))
  var count = 0

  val dict = scala.io.Source.fromFile(new File("/usr/share/dict/words"))
  var dictwords:Set[String] = Set()
  for(word <- dict.getLines()){
    dictwords+=word.toLowerCase
  }
  //val rexa_corrected = new java.io.PrintWriter(new File("/Users/lvnair/Documents/code/Kmeans/rexa_corrected"))
  val rexa_corrected = new java.io.PrintWriter(new File("/iesl/canvas/lvnair/rexa_hyphencorrected.txt"))
 for (line <- source.getLines()) {
   try{
    val d = new Document(line)
    d.annotators(classOf[Token]) = UnknownDocumentAnnotator.getClass
    val b = new DeterministicTokenizer()
    val doc = b.process(d)
    //doc.tokens.foreach(t=>print(t.string+" "))
    //println()
    val doc2 = new HyphenConcatenator(dictwords, useTokens = true ).process(doc)
    //println(doc2.tokenCount)
    //println("After")
    //doc2.tokens.foreach(t=>print(t.string+" "))

    try {

      doc2.tokens.foreach(t=>rexa_corrected.write(t.string+" "))
      rexa_corrected.write("\n")
    }
    //println()

    //val doc = Document.fromString(WordSeqDomain, inputdocuments+":"+count, line, segmenter = mySegmenter)

    //val sampleString = "This is a test document"
    /*val doc1 = new cc.factorie.app.nlp.Document(line)
    BasicTokenizer.process(doc1)
    val doc = new StringBuilder()

    for (token <- doc1.tokens) {
      doc.append(token.string+" ")
      //println(token)

    }
    println(doc) */
    //val sent = HyphenConcatenator.concatHyphenatedWords(line.toString())

    //pw.write(sent+"\n")

   }
   catch{
     case e:StackOverflowError => println("Error in line : "+line)
   }
 }
   rexa_corrected.close()
   //pw.close


 }
}
