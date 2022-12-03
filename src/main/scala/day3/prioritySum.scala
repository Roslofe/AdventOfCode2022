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
  /** compare3 does the same thing as compareItems, but with three inputs. */
  private def compare3(a: Array[Char], b: Array[Char], c: Array[Char]) = a.intersect(b).intersect(c).distinct.map(x => priorityValues.indexOf(x) + 1).sum

  /** findPriority reads the files, as well as calls the other functions.
   * @param first   Determines whether the first or second half of the exercise is being evaluated.
   * @return        The sum of all the common inputs */
  private def findPriority(first: Boolean) =
    try
      val file = File("./src/main/scala/day3/data.txt")
      val fileReader = FileReader(file)
      val lineReader = BufferedReader(fileReader)
      //Attempt to read file data
      try
        var line = lineReader.readLine()
        var currScore = 0
        /** Stores the three lines used in each evaluation. */
        val elfGroup = Array.fill(3)(Array('a'))
        var elfIndex = 0
        while line != null do
          val individuals = line.toCharArray
          if first then
            //Determine size of each half
            val separatingSize = individuals.length / 2
            currScore += compareItems(individuals.take(separatingSize), individuals.takeRight(separatingSize))
          else if elfIndex < 3 then
            //Add the current Array to elfGroup for later evaluation
            elfGroup.update(elfIndex, individuals)
            elfIndex += 1
          else
            //Evaluate the Arrays in elfGroup to get the score
            currScore += compare3(elfGroup(0), elfGroup(1), elfGroup(2))
            elfGroup.update(0, individuals)
            elfIndex = 1
          line = lineReader.readLine()
        currScore += compare3(elfGroup(0), elfGroup(1), elfGroup(2))
        currScore
      finally
        fileReader.close()
        lineReader.close()
    catch
      case notFound: FileNotFoundException => println("File wasn't connected correctly")
      case _ => println("Problems with reading the file")
  end findPriority

  println(s"Sum of duplicate items found: ${findPriority(true)}")
  println(s"Sum of group identifiers: ${findPriority(false)}")


end prioritySum