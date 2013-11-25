package edu.cs.umass
import cc.factorie.app.nlp._
import scala.io.Source




import com.nytlabs.corpus.NYTCorpusDocumentParser
import java.io._
import org.xml.sax.SAXException
import javax.xml.parsers.{ParserConfigurationException, DocumentBuilderFactory}
import org.w3c.dom.{Document => XMLDoc}
import cc.factorie.app.nlp.Document
import collection.mutable.ArrayBuffer
import scala.collection.JavaConversions._

import java.io._

/**
 * Created with IntelliJ IDEA.
 * User: lvnair
 * Date: 10/1/13
 * Time: 1:38 PM
 * To change this template use File | Settings | File Templates.
 */




object   NYDataCovertorLDA{

  def getListOfFiles(directoryName: String): List[String] = {
    val directory = new File(directoryName)
    val files = directory.listFiles  // this is File[]
    val fileNames = ArrayBuffer[String]()
    for (file <- files) {
      if (file.isFile()) {
        fileNames += file.getName()
      }
    }
    return fileNames.toList
  }

  def getListOfSubDirectories(directoryName: String): List[String] = {
    val directory = new File(directoryName)
    val files = directory.listFiles  // this is File[]
    val dirNames = ArrayBuffer[String]()
    for (file <- files) {
      if (file.isDirectory()) {
        dirNames += file.getName()
      }
    }
    return dirNames.toList
  }

  def main(args:Array[String]) {
   val startyear = "1987"
   val endyear = "2007"
   val currentyear = args{0}
   val maindir = "/iesl/canvas/sameer/dat/nyt/data"

   //val testdir = "/home/lvnair"
   val outputdir = "/iesl/canvas/lvnair/NYDataText/1988"
   //val dirpath = "/iesl/canvas/sameer/dat/nyt/data/1987/01/01"
   val samplefile = "/home/lvnair/temp1.xml"

   //val file = new java.io.File(samplefile)

    //for(line <- Source.fromFile(file).getLines().take(2)) println(line)
   /*val document =  cc.factorie.app.nlp.load.LoadNYTimesXML.fromFile(file)

   for(doc <- document){
     //for(sent <- doc.sentences) {
       println(doc.sentences.);
     //}
   }  */
   /*for(file <- new java.io.File(dirpath).listFiles()){
       println(count +" : "+file.getName)
      count=count+1
   }  */

   //LoadNYTimesXML.fromFile()

   val dirlist = getListOfSubDirectories(maindir+"/"+currentyear)
   for (dir<- dirlist){
      val newdirpath =  maindir+"/"+currentyear+"/"+dir
      val lowerdirlist = getListOfSubDirectories(newdirpath)
      for(lowerdir<-lowerdirlist ){
         val finaldirpath = newdirpath+"/"+lowerdir
         val files = getListOfFiles(finaldirpath)
         for(inputfile <- files){
           if(inputfile.matches(".*\\.xml$")){
           var input = {
             new StringBuilder(inputfile)
           }
             input = input.dropRight(4)
             input = input.append(".txt")
             val input_final = input.toString()
             //println("Input "+input_final)
           val filepath = finaldirpath+"/"+inputfile
           //println(filepath)
           val outputfile =  outputdir+"/"+currentyear+"_"+dir+"_"+lowerdir+"_"+input_final
           //println("file"+outputfile)
           //println(filepath.toString())
           val documents = NYTLoader.fromFile(filepath.toString())

           for(doc <- documents){

             //println("inside")

             var textfile = new PrintWriter(new File(outputfile))

             textfile.write(doc.string)
             textfile.close()
           }
          }
       }
      }
   }


   /*val documents =   NYTLoader.fromFile(samplefile);
   for(doc <- documents){
        println(doc.string)
    }  */

   }
}




/**
 * Created by IntelliJ IDEA.
 * User: lmyao
 * Date: 6/11/13
 * Time: 12:25 PM
 * To change this template use File | Settings | File Templates.
 */

object NYTLoader {
  val parser = new NYTCorpusDocumentParser

  private def loadNonValidating(file: File): XMLDoc = {
    var document: XMLDoc = null
    val sb = new StringBuffer()
    try {
      val in = new BufferedReader(new InputStreamReader(
        new FileInputStream(file), "UTF8"));
      var line: String = null;
      while ( {
        (line = in.readLine());
        line
      } != null) {
        sb.append(line + "\n");
      }
      var xmlData = sb.toString();
      xmlData = xmlData.replace("<!DOCTYPE nitf "
        + "SYSTEM \"http://www.nitf.org/"
        + "IPTC/NITF/3.3/specification/dtd/nitf-3-3.dtd\">", "");
      //println("XMLData "+xmlData);
      document = parseStringToDOM(xmlData, "UTF-8", file);

      in.close
      return document;
    } catch {
      case e: UnsupportedEncodingException => {
        e.printStackTrace();
        System.out.println("Error loading file " + file + ".");
      }
      case e: FileNotFoundException => {
        e.printStackTrace();
        System.out.println("Error loading file " + file + ".");
      }
      case e: IOException => {
        e.printStackTrace();
        System.out.println("Error loading file " + file + ".");
      }
    }
    return null;
  }

  private def parseStringToDOM(s: String, encoding: String, file: File): XMLDoc = {
    try {
      val factory = DocumentBuilderFactory.newInstance();
      factory.setValidating(false);
      val is = new ByteArrayInputStream(s.getBytes(encoding));
      val doc = factory.newDocumentBuilder().parse(is);
      return doc;
    } catch {
      case e: SAXException => {
        e.printStackTrace();
        sys.error("Exception processing file " + file + ".");
      }
      case e: ParserConfigurationException => {
        e.printStackTrace();
        sys.error("Exception processing file " + file + ".");
      }
      case e: IOException => {
        e.printStackTrace();
        sys.error("Exception processing file " + file + ".");
      }
    }

  }



  def parseNYTCorpusDocumentFromFile(file: File) = {
    parser.parseNYTCorpusDocumentFromDOMDocument(file, loadNonValidating(file))
  }

  def files(directory: File): Seq[File] = {
    if (!directory.exists) throw new Error("File " + directory + " does not exist")
    if (directory.isFile) return Seq(directory)
    val result = new ArrayBuffer[File]
    for (entry: File <- directory.listFiles) {
      if (entry.isFile) result += entry
      else if (entry.isDirectory) result ++= files(entry)
    }
    result
  }

  //junk pattern Schedule, Correction, Statistics, List    todo: we could keep weddings and engagements, some of them are useful
  def isJunk(file: File) = {
    var res = false
    val nytDoc = NYTLoader.parseNYTCorpusDocumentFromFile(file)
    val types = nytDoc.getTypesOfMaterial ++ nytDoc.getGeneralOnlineDescriptors //++ nytDoc.getTaxonomicClassifiers
    for (typeString <- types) {


      if (typeString.contains("Paid Death Notice") || typeString.contains("Paid Memorial Notice")
        || typeString.contains("Weddings and Engagements") || typeString.toLowerCase.contains("correction")
        || typeString.contains("Schedule") || typeString.contains("Statistics") || typeString.contains("List")
      ) res = true
    }

    res
  }

  def extractTextWithNYTParser(file: File) = {
    val nytDoc = NYTLoader.parseNYTCorpusDocumentFromFile(file)
    //println("Body "+nytDoc.getBody)
    var nytAbstract = nytDoc.getArticleAbstract
    //println("abstract "+nytAbstract)
    if (nytAbstract != null && nytAbstract.matches(".*\\([A-Z]\\)$")) nytAbstract = nytAbstract.replaceAll("\\s\\([A-Z]\\)$", "")
    val body = nytDoc.getBody
    if (nytAbstract != null && body != null) nytAbstract + ".\n" + nytDoc.getBody
    else body
  }

  def fromFile(file: String): Seq[Document] = {
    val f = new File(file)


    if (!NYTLoader.isJunk(f) && f.getName.matches(".*\\.xml$")) {
      val text = NYTLoader.extractTextWithNYTParser(f)

      if (text != null) {
        Seq(new Document(text).setName(f.getName))
      } else Seq[Document]() // end of one document
    } else Seq[Document]()
  }

  def process(dirs: Seq[String]): Seq[Document] = {
    val filelist: Seq[File] = dirs.map(x => files(new File(x))).flatMap(x => x)
    val docs = new ArrayBuffer[Document]
    //process each nyt file
    for (file: File <- filelist) {
      docs ++= fromFile(file.getAbsolutePath)
    }
    //this is just something to make it compatible with some stuff elsewhere in tackbp
    //docs.foreach(d => d.attr += new SGMSource("nyt-xml"))
    docs
  }

}






