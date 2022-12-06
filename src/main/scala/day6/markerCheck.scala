package day6
import java.io._

/** markerCheck reads an input stream, and detects when a four/fourteen-character marker with distinct members is processed.
 * For more information, see: https://adventofcode.com/2022/day/6 */
object markerCheck extends App:

  /** Evaluates the stream, and ends processing once a four/fourteen-character marker has been found.
   * @param first   Parameter determining which half of the exercise is being evaluated
   * @return        The number of characters needed to process prior to finding a marker
   */
  private def findMarker(first: Boolean) =
    try
      val file = File("./src/main/scala/day6/data.txt")
      val fileReader = FileReader(file)
      val dataReader = BufferedReader(fileReader)
      //Attempt to read file data
      try
        //Store the marker, as well as the number of markers processed
        var markerNum = -1
        var markersProcessed = 0
        //Which number of characters is being evaluated, depends on the half of the exercise
        val markerLenght = if first then 4 else 14
        //Characters that could be part of a marker
        val prevChars = scala.collection.mutable.ListBuffer[Char]()
        var currentChar = dataReader.read()
        while currentChar != -1 && markerNum == -1 do
          val charValue = currentChar.toChar
          markersProcessed += 1
          //Update the marker number if the current sequence is a marker
          if !prevChars.contains(charValue) && prevChars.distinct.size == prevChars.size && markersProcessed >= markerLenght then
            markerNum = markersProcessed
          else
            if prevChars.size == markerLenght - 1 then
              prevChars.remove(0)
            prevChars.append(charValue)
          currentChar = dataReader.read()
        markerNum
      finally
        fileReader.close()
        dataReader.close()
    catch
      case notFound: FileNotFoundException => println("File wasn't connected correctly")
      case _ => println("Problems with reading the file")
  end findMarker

  println(s"First four-character marker found after character: ${findMarker(true)}")
  println(s"First fourteen-character marker found after character: ${findMarker(false)}")

end markerCheck

