package edu.cs.umass.Kmeans

import scala.util.Random
import java.io.File
import cc.factorie._
import cc.factorie.la.{ProxyGrowableDenseTensor1, GrowableSparseTensor1, Tensor1}
import cc.factorie.Tensor1
import cc.factorie.variable._
import java.util
import collection.mutable._
import cc.factorie.la.{DenseTensor1, Tensor}


import cc.factorie.app.topics.lda.Document
import java.util.HashMap
import cc.factorie.app.chain.Observations._
import  cc.factorie.app.nlp
import scala.collection.mutable
import scala.collection.immutable.Range




class KMeansToken{


  object TokenDomain extends CategoricalVectorDomain[String]
  class Features(val token:String) extends BinaryFeatureVectorVariable[String] { def domain = TokenDomain; override def skipNonCategories = true }

  def initFeatures(token: String,count:Int): Features = {
      val features = new Features(token)
      features += "Token="+token
      features += "DocumentId=" + count
      //println(features)
   features
   }
}


object KMeansToken {

    def main(args:Array[String]) {
      val kmeansToken = new  KMeansToken()
      var points = scala.collection.mutable.ArrayBuffer[Tensor1]()
      var tokens = scala.collection.mutable.ArrayBuffer[String]()
      var topic_list = new mutable.HashMap[Int,ArrayBuffer[String]]()
      val k = args(0).toInt
      println("No of topics "+args(0))
      val docfile = args(1)
      println("Reading documents")
      val output = args(2)
      // the features are type id and document id
      val topwordsfile = new java.io.PrintWriter(new File(output))



      object WordSeqDomain extends CategoricalSeqDomain[String]
      def tokenRegex = "\\p{Alpha}+"
      val mySegmenter = new cc.factorie.app.strings.RegexSegmenter(tokenRegex.r)
      val topic_words_file = scala.io.Source.fromFile(new File(docfile))
      var count=0

      // generate features for each token
      for (line <- scala.io.Source.fromFile(new File(docfile)).getLines()) {
      count=count+1
      //val words = cc.factorie.app.strings.alphaSegmenter(line).filter(!cc.factorie.app.nlp.lexicon.StopWords.containsWord(_)).map(_.toLowerCase).toSeq
      //val words =  cc.factorie.app.strings.alphaSegmenter(line).filter(!cc.factorie.app.nlp.lexicon.StopWords.containsWord(_)).map(_.toLowerCase).toString
      //println(words)
      // val doc = new cc.factorie.app.nlp.Document(words)
      //for(tok <- doc.tokens){
      val doc = Document.fromString(WordSeqDomain, topic_words_file+":"+count, line, segmenter = mySegmenter)
      for(type_values <- doc.ws.categoryValues) {
        val features = kmeansToken.initFeatures(type_values,count)
        //println(features.value)
        tokens += type_values
        points += features.value
       }



    }


     //val finalpoints = ParallelKMeans.bestOfN(points.toSeq,k,10,points(0).length,2)
     println("No of tokens " +tokens.length)
     println("No of features "+ points(0).size)

     val finalpoints = ParallelKMeans.goodInitialize(points.toSeq,k,points(0).length,500,1.0)
     //println(finalpoints.toSeq)
     //println("Best points "+finalpoints.toArray.zipWithIndex.par.groupBy(x=> x._1._1))
     finalpoints.toArray.zipWithIndex.par.groupBy(x=> x._1._1).map(group => {

       val topic_group = group._1
       //println(topic_group)
       val examples = group._2
       //println("eg "+examples)
       val topic_groups = examples.toArray.sortWith((el1, el2) => (el1._1._2 < el2._1._2)).toSeq
       //println(topic_groups)
       val topic_words = new ArrayBuffer[String]()
       for(i<- 0 to topic_groups.length-1) {
         topic_words+=tokens(topic_groups.apply(i)._2)

       }
       println("Topic "+topic_group +"="+topic_words.mkString(","))
       topwordsfile.write(topic_group+" "+topic_words.mkString(" ")+"\n")

       //examples.foreach(eg => println(eg._1))
     }  )




      /*for (x <- finalpoints.zipWithIndex) {
        val buf = new ArrayBuffer[String]()

        if(!topic_list.contains(x._1._1)) {topic_list.put(x._1._1,buf+= (tokens(x._2)))}
        else{topic_list.put(x._1._1,topic_list.apply(x._1._1)+=tokens(x._2))}
      }
      //println(topic_list)
      topic_list.foreach {keyVal => println("Topic "+keyVal._1 + " = " + keyVal._2.mkString(","))}
        //println("String #" + x._2 + " is " + x._1 + x._1._1)  */

    //select k initial clusters randomly
    /*val rand = new Random()
    var i=0
    while (i<k){
      val random_index = rand.nextInt(points.length)
      val result = points(random_index)
      centers+=result
      i=i+1
    }  */
    //println(centers)
    topwordsfile.close()
  }
}


