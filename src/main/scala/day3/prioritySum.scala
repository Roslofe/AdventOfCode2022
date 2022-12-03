package day3
import java.io._

/** pioritySum calculates a numerical value based on the common characters in Arrays. For more information, see: https://adventofcode.com/2022/day/3 */
object prioritySum extends App:
  /** priorityValues stoes each of the possible input characters. It is used to determine priority values. */
  private val priorityValues = ('a' to 'z').toArray ++ ('A' to 'Z').toArray


  /** compareItems evaluates the two halves of the input, and returns the sum of the common priorities.
   * @param a   The first half of the input
   * @param b   The second half of the input
   * @return    The sum of the priorities of the common values
   * */
  private def compareItems(a: Array[Char], b: Array[Char]) = a.intersect(b).distinct.map(x => priorityValues.indexOf(x) + 1).sum

  /** findPriority reads the files, as well as calls the other functions.
   * @return  The sum of all the common inputs */
  private def findPriority() =
    try
      val file = File("./src/main/scala/day3/data.txt")
      val fileReader = FileReader(file)
      val lineReader = BufferedReader(fileReader)
      //Attempt to read file data
      try
        var line = lineReader.readLine()
        var currScore = 0
        while line != null do
          val individuals = line.toCharArray
          //Determine size of each half
          val separatingSize = individuals.length / 2
          currScore += compareItems(individuals.take(separatingSize), individuals.takeRight(separatingSize))
          line = lineReader.readLine()
        currScore
      finally
        fileReader.close()
        lineReader.close()
    catch
      case notFound: FileNotFoundException => println("File wasn't connected correctly")
      case _ => println("Problems with reading the file")
  end findPriority

  println(s"Sum of duplicate items found: ${findPriority()}")


end prioritySum