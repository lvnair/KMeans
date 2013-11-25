package edu.cs.umass

import scala.xml._
import scala.io.Source._
import javax.xml.parsers.DocumentBuilderFactory
import javax.xml.parsers.DocumentBuilder
import org.w3c.dom.Document
import javax.xml.xpath.XPath
import javax.xml.xpath.XPathConstants
import javax.xml.xpath.XPathExpression
import javax.xml.xpath.XPathExpressionException
import javax.xml.xpath.XPathFactory
import java.io.File

object RexaDataConvertor {
  def main(args:Array[String]) {

    val rexacorpuspath ="/iesl/canvas/saunders/datasets/rexa-corpus"
    val meta_files = "/iesl/canvas/lvnair/rexa_meta_files.txt"


    scala.xml.XML.loadFile("/iesl/canvas/lvnair/rexa_meta_files.txt")




  }

}

