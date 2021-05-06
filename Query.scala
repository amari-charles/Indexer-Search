package search.sol

import scala.math.log

import java.io._
import search.src.{FileIO, PorterStemmer, StopWords}

import scala.collection.mutable
import scala.collection.mutable.HashMap
import scala.util.matching.Regex

/**
 * Represents a query REPL built off of a specified index
 *
 * @param titleIndex    - the filename of the title index
 * @param documentIndex - the filename of the document index
 * @param wordIndex     - the filename of the word index
 * @param usePageRank   - true if page rank is to be incorporated into scoring
 */
class Query(titleIndex: String, documentIndex: String, wordIndex: String,
            usePageRank: Boolean) {

  // Maps the document ids to the title for each document
  private val idsToTitle = new HashMap[Int, String]

  // Maps the document ids to the euclidean normalization for each document
  private val idsToMaxFreqs = new HashMap[Int, Double]

  // Maps the document ids to the page rank for each document
  private val idsToPageRank = new HashMap[Int, Double]

  // Maps each word to a map of document IDs and frequencies of documents that
  // contain that word
  private val wordsToDocumentFrequencies = new HashMap[String, HashMap[Int, Double]]

  val wordsRegex = new Regex("""[^\W_]+'[^\W_]+|[^\W_]+""") //Regex to just match words

  def outputDocuments(scoreToId:mutable.HashMap[Double, Int], numOfDocs: Int): Array[Int] ={
    //Create output array with a slot for each documents id.
    var tempArray = new Array[Double](numOfDocs)
    tempArray = scoreToId.keys.toArray.sorted
    tempArray = tempArray.reverse
    //tempArray.foreach(score => println("score: " + score))
    val outputArray = new Array[Int](numOfDocs)
    for(i <- 0 until numOfDocs){
      outputArray(i) = scoreToId(tempArray(i))
    }
    outputArray
  }

  /**
   * Handles a single query and prints out results
   *
   * @param userQuery - the query text
   */
  private def query(userQuery: String) {
    // TODO : Fill this in
    val linkMatchesIterator = wordsRegex.findAllMatchIn(userQuery)
    val wordArray = linkMatchesIterator.toArray.map { aMatch => aMatch.matched.trim }
    val stemmed = PorterStemmer.stemArray(wordArray.filterNot(word => StopWords.isStopWord(word)))
    val idsToScore = new mutable.HashMap[Int,Double]
    var scoreToId = new mutable.HashMap[Double, Int]



    for (word <- stemmed){
      wordsToDocumentFrequencies.get(word) match{
        case Some(map) =>
          for (pair <- map){
            //Calculate the idf for this doc
            val idf:Double = log(idsToTitle.size.toDouble / map.size.toDouble)
            //Calculate the tf for this doc
            val tf = pair._2 / idsToMaxFreqs(pair._1)
            //The score for this document and term is the product of the prev two, (Summed Later in the code)
            val score = tf * idf
            idsToScore.get(pair._1) match {
              case Some(initialScore) =>
                //Add score to initial
                idsToScore(pair._1) = initialScore + score
                //Remove old score in the inverseHashMap
                scoreToId = scoreToId.-(initialScore)
                //Add back the new value
                scoreToId(idsToScore(pair._1)) = pair._1
              case None =>
                if (score != 0 || usePageRank) {
                  idsToScore(pair._1) = score
                  scoreToId(score) = pair._1
                }
            }
          }
        case None => //The term doesn't appear in the document therefore there is no score
      }
      //If we implement pagerank add them to its score
      if(usePageRank){
        for(pair <- idsToScore){
          //Add to rank
          idsToScore(pair._1) = pair._2 + idsToPageRank(pair._1)
          //Remove old score
          scoreToId = scoreToId.-(pair._2)
          //Add new score
          scoreToId(pair._2 + idsToPageRank(pair._1)) = pair._1
        }
      }
      if(scoreToId.size >= 10){
        printResults(outputDocuments(scoreToId,10))
      }else if(scoreToId.size > 0 && scoreToId.size < 10){
        printResults(outputDocuments(scoreToId,scoreToId.size))
      }else{
        println("No documents match your search")
      }
    }

  }

  /**
   * Format and print up to 10 results from the results list
   *
   * @param results - an array of all results to be printed
   */
  private def printResults(results: Array[Int]) {
    for (i <- 0 until Math.min(10, results.size)) {
      println("\t" + (i + 1) + " " + idsToTitle(results(i)))
    }
  }

  /*
   * Reads in the text files.
   */
  def readFiles(): Unit = {
    FileIO.readTitles(titleIndex, idsToTitle)
    FileIO.readDocuments(documentIndex, idsToMaxFreqs, idsToPageRank)
    FileIO.readWords(wordIndex, wordsToDocumentFrequencies)
  }

  /**
   * Starts the read and print loop for queries
   */
  def run() {
    val inputReader = new BufferedReader(new InputStreamReader(System.in))

    // Print the first query prompt and read the first line of input
    print("search> ")
    var userQuery = inputReader.readLine()

    // Loop until there are no more input lines (EOF is reached)
    while (userQuery != null) {
      // If ":quit" is reached, exit the loop
      if (userQuery == ":quit") {
        inputReader.close()
        return
      }

      // Handle the query for the single line of input
      query(userQuery)

      // Print next query prompt and read next line of input
      print("search> ")
      userQuery = inputReader.readLine()
    }

    inputReader.close()
  }
}

object Query {
  def main(args: Array[String]) {
    try {
      // Run queries with page rank
      var pageRank = false
      var titleIndex = 0
      var docIndex = 1
      var wordIndex = 2
      if (args.size == 4 && args(0) == "--pagerank") {
        pageRank = true;
        titleIndex = 1
        docIndex = 2
        wordIndex = 3
      } else if (args.size != 3) {
        println("Incorrect arguments. Please use [--pagerank] <titleIndex> "
          + "<documentIndex> <wordIndex>")
        System.exit(1)
      }
      val query: Query = new Query(args(titleIndex), args(docIndex), args(wordIndex), pageRank)
      query.readFiles()
      query.run()
    } catch {
      case _: FileNotFoundException =>
        println("One (or more) of the files were not found")
      case _: IOException => println("Error: IO Exception")
    }
  }
}
