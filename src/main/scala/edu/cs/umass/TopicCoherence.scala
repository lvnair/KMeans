package edu.cs.umass

import java.lang.String
import java.util.HashMap
import scala.collection._
import cc.factorie.variable.CategoricalSeqDomain

import java.io.{File, Reader}
import cc.factorie.app.strings.StringSegmenter
import cc.factorie.app.topics.lda.{Document, Doc}
import scala.math._
import scala.util.control.Breaks._
import scala.Predef._
import scala.collection.Set


object TopicCoherence {
  def main(args:Array[String]){
    /*object opts extends cc.factorie.util.DefaultCmdOptions {
      val input =     new CmdOption("input-file", "", "DOCUMENTFILE", "File containing documents one for each line.")
      val topicfile =     new CmdOption("topic-file", "", "TOPWORDSFILE", "File containing top words associated with each topic delimited by space. Each line corresponds to a topic.")
      val output =    new CmdOption("output-file", "", "COHERENCEOUTPUT", "File to write the coherence value associated with each topic")
      val numtopwords = new CmdOption("num-top-words", 10, "top word counts", "number of top words to considered for calculating coherence metric")
    } */
    //val inputdocuments = "/iesl/canvas/lvnair/rexa_corpus/rexa_corpus_fullpapers.txt"
    //val inputdocuments = "/Users/lvnair/Documents/code/Kmeans/rexa_test"
    val topic_highfrequencyword_list = new mutable.HashMap[String,Vector[String]]
    //.withDefaultValue(Nil)

    /*val input = "/Users/lvnair/Documents/code/Kmeans/rexa_test"
    val topicfile ="/Users/lvnair/Documents/code/Kmeans/LDA_top_words_final"
    val output ="/Users/lvnair/Documents/code/Kmeans/LDA_coherence"   */
    val input = "/iesl/canvas/lvnair/rexa_corpus/rexa_hyphencorrected_abstract_50K.txt"
    //val topicfile ="/iesl/canvas/lvnair/rexa_corpus/LDA_top_words100_1000topics_finaliteration"
    //val output ="/iesl/canvas/lvnair/rexa_corpus/LDA_coherence_hash_1000topics_20.txt"
    val topicfile = args(0)
    val output = args(1)
    val wordcount =20



    var types = scala.collection.mutable.Set[String]()
    var type_pairs =  scala.collection.mutable.Set[String]()
    /*topic_highfrequencyword_list("topic1") ::= "ping"
    topic_highfrequencyword_list("topic1") ::= "proved"
    topic_highfrequencyword_list("topic2") ::= "school" */


     // no of high probability words in each topics
     //var wordcount = 3
     val codocument_frequencies = new HashMap[String,Int]
     val document_frequencies = new HashMap[String,Int]
     val coherence = new HashMap[String,Double]
     object WordSeqDomain extends CategoricalSeqDomain[String]
     //def newDocument(domain:CategoricalSeqDomain[String], name:String, contents:Reader): Doc = Document.fromReader(domain, name, contents)
     def tokenRegex = "\\p{Alpha}+"
     val mySegmenter = new cc.factorie.app.strings.RegexSegmenter(tokenRegex.r)

     var count = 0



    /*if(!(opts.input.wasInvoked) || !(opts.topicfile.wasInvoked) || !(opts.output.wasInvoked) || !(opts.numtopwords.wasInvoked) ){
      System.err.println("Incorrect inputs.. Require --input-file , --topic-file , --output-file , --num-top-words options")
      System.exit(1)
    }*/

     // reading topic file
    println("Reading topic file")
    val topic_words_file = scala.io.Source.fromFile(new File(topicfile))
    for (line <- topic_words_file.getLines()) {
      val topic_words = line.split(" ")
      val topwords = topic_words.slice(1,wordcount+1)
      var topword:Vector[String] = Vector()
      for(j<- topwords){
        //print(j+" ")
        topword = topword :+ j
      }


      //println(topword)
      /*var topword:Vector[String] = Vector()
      for(word <- topwords){
        topword+=word
      } */
      topic_highfrequencyword_list("topic"+topic_words(0)) = topword
    }
     println(topic_highfrequencyword_list)
     var numtopics = topic_highfrequencyword_list.size


     val source = scala.io.Source.fromFile(new File(input))
     println("Intializing term-document frequencies")
     //Initializing term document frequencies
     for(i<- 0 to numtopics-1){
      for(term <- topic_highfrequencyword_list.apply("topic"+i)){
        if(!document_frequencies.containsKey(term)){
          types += term
          document_frequencies.put(term,0)
        }
      }
     }
    println("Types "+types)
    // Initialising term pair document frequencies
    println("Initialising term pair document frequencies")
    for(i <- 0 to numtopics-1){
      for(m <- 1 to wordcount-1){
       val vm =topic_highfrequencyword_list.apply("topic"+i)(m)
       for(l<- 0 to m-1){
          val vl = topic_highfrequencyword_list.apply("topic"+i)(l)
          if((!codocument_frequencies.containsKey(vm+" "+vl)) && (!codocument_frequencies.containsKey(vl+" "+vm))){
            type_pairs += vm+" "+vl
            codocument_frequencies.put(vm+" "+vl,0)
          }

       }
      }
    }

    println("Document frequencies " +document_frequencies)
    println("Co-document frequencies "+codocument_frequencies)
    println("Reading documents")

     for (line <- source.getLines()) {
     count=count+1
     println("Line count :" +count)
      val doc = Document.fromString(WordSeqDomain, input+":"+count, line, segmenter = mySegmenter)
      val doc_hash = new HashMap[String,Int]
      for(type_values <- doc.ws.categoryValues) {
        if(!doc_hash.containsKey(type_values)){
            doc_hash.put(type_values,0)
        }

      }

       for(vl<-types) {
        if(doc_hash.containsKey(vl)){
          val newcount = document_frequencies.get(vl)+1
          document_frequencies.put(vl,newcount)
        }
       }

       for(pairs <- type_pairs){
         //println(pairs)
         val pair = pairs.split(" ")
         //println(pair(0))
         if(doc_hash.containsKey(pair(0)) && doc_hash.containsKey(pair(1))){
           val newcount = codocument_frequencies.get(pairs)+1
           codocument_frequencies.put(pairs,newcount)
         }
       }

    }





    var sum=0.0
    val coherencefile = new java.io.PrintWriter(new File(output))
      for(i <- 0 to numtopics-1){
       var coherencevalue:Double = 0.0

       for(m <- 1 to wordcount-1){
         val vm =topic_highfrequencyword_list.apply("topic"+i)(m)
         for(l<- 0 to m-1){
             val vl = topic_highfrequencyword_list.apply("topic"+i)(l)

             //println(vm+" "+vl +" "+codocument_frequencies.get(vm+" "+vl))
             //println(vl +" "+document_frequencies.get(vl))

             val value = (codocument_frequencies.get(vm+" "+vl) + 1).toDouble/document_frequencies.get(vl).toDouble
             //println(value)
             //coherencevalue +=  log((codocument_frequencies.get(vm+" "+vl) + 1)/document_frequencies.get(vl))
             coherencevalue +=  log(value)
             //coherence.put("topic"+i)
           }
         }
        coherence.put("topic"+i,coherencevalue)
        sum += coherencevalue
        println("topic"+i,coherencevalue)
        coherencefile.write(i+" "+coherencevalue+"\n")
       }




   val average = sum.toDouble/numtopics.toDouble
   coherencefile.write("Average = "+average+"\n")
   source.close()
   topic_words_file.close()
   coherencefile.close()
   //println(coherence)

 }
}

//val sampleString = "This is a test document"
/*val doc1 = new cc.factorie.app.nlp.Document(line)
BasicTokenizer.process(doc1)
val doc = new StringBuilder()

for (token <- doc1.tokens) {
     doc.append(token.string+" ")
     //println(token)

}  */
