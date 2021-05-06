package search.sol

import search.src.{FileIO, PorterStemmer, StopWords}

import java.io.{BufferedWriter, FileNotFoundException, FileWriter}

import scala.collection.mutable.HashMap
import scala.collection.mutable
import scala.util.matching.Regex
import scala.xml.{Node, NodeSeq}
import scala.collection.mutable.ListBuffer
import java.io.FileNotFoundException
import java.io.IOException
/**
 * Provides an XML indexer, produces files for a querier
 *
 * @param inputFile - the filename of the XML wiki to be indexed
 */
class Index(val inputFile: String) {

  //----------------------------------------------------------------
  // Parse data and establish key variables
  //----------------------------------------------------------------

  val mainNode: Node = xml.XML.loadFile(inputFile)
  val pageSeq: NodeSeq = mainNode \ "page" // NodeSeq of the document's texts in order
  val generalRegex = new Regex("""\[\[[^\[]+?\]\]|[^\W_]+'[^\W_]+|[^\W_]+""") //Regex to just match words
  var idsToTitles = new mutable.HashMap[Int,String]
  var idsToLinks = new mutable.HashMap[Int,List[String]] withDefaultValue List.empty[String]
  var idsToMaxCount = new mutable.HashMap[Int,Double]
  var wordsToDocFrequency = new mutable.HashMap[String,mutable.HashMap[Int,Double]]
  var titleList = new ListBuffer[String]
  for(page <- pageSeq){val title = (page \ "title").text.trim()
    titleList += title
  }

  val numOfDocs: Int = titleList.size
  def wordHandler(list:List[String], word: String): List[String] ={
    var outputList = list
    if(!StopWords.isStopWord(word)) {
      outputList = list.::(PorterStemmer.stem(word).toLowerCase())
    }
    outputList
  }
  def counterHelper(maxCount: Double, word: String, id: Int): Double = {
    var maxCountOutput = maxCount
    wordsToDocFrequency.get(word) match {
      case Some(map) => map.get(id) match {
        case Some(frequency) =>
          map(id) = frequency + 1
          if (frequency + 1 > maxCount) {
            maxCountOutput = frequency + 1
          }
        case None => map(id) = 1
      }
      case None => wordsToDocFrequency(word) = mutable.HashMap(id -> 1)
    }
    maxCountOutput
  }
  for (page <- pageSeq){
    var maxCount: Double = 0
    val title:String = (page  \ "title").text.trim()
    val id:Int = (page  \ "id").text.trim().toInt
    idsToTitles += (id -> title)
    val linkMatchesIterator = generalRegex.findAllMatchIn(page.text)
    val words = linkMatchesIterator.toList.map { aMatch => aMatch.matched }
    for(word <- words) {
      var text: List[String] = List.empty[String]
      if (word.startsWith("[[")) {
        if (word.contains("|")) {
          val tempWord = word.substring(word.indexOf('|') + 1, word.length - 2)
          val linkMatchesIterator2 = generalRegex.findAllMatchIn(tempWord)
          val words = linkMatchesIterator2.toList.map { aMatch => aMatch.matched }
          for(word <- words){ wordHandler(text, word) }
          val tempWord2 = word.substring(2, word.indexOf('|'))
          if (titleList.contains(tempWord2) && (idsToTitles(id) != tempWord2)) {
            idsToLinks(id) = tempWord2 :: idsToLinks(id)
          }
        } else if (word.contains(":")) {
          val tempWord = word.substring(2, word.length - 2)
          val linkMatchesIterator2 = generalRegex.findAllMatchIn(tempWord)
          val words = linkMatchesIterator2.toList.map { aMatch => aMatch.matched }
          for(word <- words){ wordHandler(text, word) }
          if (titleList.contains(tempWord) && (idsToTitles(id) != tempWord)) {
            idsToLinks(id) = tempWord :: idsToLinks(id)
          }
        } else {
          val tempWord = word.substring(2, word.length - 2)
          val linkMatchesIterator2 = generalRegex.findAllMatchIn(tempWord)
          val words = linkMatchesIterator2.toList.map { aMatch => aMatch.matched }
          for(word <- words){ wordHandler(text, word) }
          if (titleList.contains(tempWord) && (idsToTitles(id) != tempWord)) {
            idsToLinks(id) = tempWord :: idsToLinks(id)
          }
        }
      } else {
        text = wordHandler(text, word.trim)
      }
      for(word <- text) {maxCount = counterHelper(maxCount, word, id)}
    }
    idsToMaxCount += (id -> maxCount)
    //END OF PAGE LOOP
  }
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /////// Page Rank
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  val initialR2: Double = 1d / numOfDocs
  var r1Hash = new mutable.HashMap[Int,Double]
  var r2Hash = new mutable.HashMap[Int,Double]
  for(pair <- idsToTitles) {
    r1Hash += (pair._1 -> 0)
    r2Hash += (pair._1 -> initialR2)
  }
  def weightHelper(idJ: Int, idK: Int, linkHash: mutable.Map[Int, List[String]]): Double ={
    var num:Double = 0
    val e:Double = .15
    if (linkHash(idK).contains(idsToTitles(idJ)) && (idK != idJ)) /*check this*/ {
      num = (e / numOfDocs) + ((1 - e) / linkHash(idK).size)
    } else if(linkHash(idK).isEmpty && (idK != idJ)){
      num = (e / numOfDocs) + ((1 - e) / (numOfDocs - 1))
    }
    else {
      num = e / numOfDocs
    }
    num
  }
  //Check distance
  def distanceCheck(r1: mutable.HashMap[Int,Double], r2: mutable.HashMap[Int,Double]): Boolean = {
    var distance: Double = 0
    val r1r2 = r1 zip r2
    for(r <- r1r2){
      val num = (r._2._2 - r._1._2) * (r._2._2 - r._1._2)
      distance += num
    }
    distance = math.sqrt(distance)
    distance > .001
  }
  //Calculating Ranks
  while(distanceCheck(r1Hash,r2Hash)){
    //r1Hash = r2Hash
    r2Hash.foreach(pair => r1Hash(pair._1) = pair._2)
    for(pair1 <- r2Hash){
      r2Hash(pair1._1) = 0
      for(pair2 <- r1Hash){
        r2Hash(pair1._1) = r2Hash(pair1._1) + (weightHelper(pair1._1,pair2._1,idsToLinks) * r1Hash(pair2._1))
      }
    }
  }
}
object Index {
  def main(args: Array[String]) {
    try {
      if (args.length == 4) {
        val file = args(0)
        val titleFile = args(1)
        val docFile = args(2)
        val wordFile = args(3)
        val index = new Index(file)
        FileIO.printTitleFile(titleFile, index.idsToTitles)
        FileIO.printDocumentFile(docFile, index.idsToMaxCount, index.r2Hash)
        FileIO.printWordsFile(wordFile, index.wordsToDocFrequency)
      } else {
        println("Incorrect number of arguments")
      }
    } catch {
      case ex1: FileNotFoundException => {
        println("Missing file exception")
      }
      case ex: IOException => {
        println("IO Exception")
      }
    }
  }
}
