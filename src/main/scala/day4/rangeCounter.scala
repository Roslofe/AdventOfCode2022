package day4
import java.io._

/** The purpose of rangeCounter is to determine the number of ranges, where one completely overlaps the other.
 * For more information, see: https://adventofcode.com/2022/day/4
 */
object rangeCounter extends App:

  /** FindOverlap evaluates each line, and checks if the provided ranges overlap.
   * @param first   A variable determining which half of the exercise is being evaluated.
   * @return        The number of overlapping ranges. */
  private def findOverlap(first: Boolean) =
    try
      val file = File("./src/main/scala/day4/data.txt")
      val fileReader = FileReader(file)
      val lineReader = BufferedReader(fileReader)
      //Attempt to read file data
      try
        var line = lineReader.readLine()
        var overlapCount = 0
        while line != null do
          //Split the data into range start- and endpoints
          val rngs = line.split(",").map(_.split("-").map(_.toInt))
          //Determine if the data overlaps partially/completely
          val overlaps = (if first then
                            val upperInsideLower = rngs(0)(0) >= rngs(1)(0) && rngs(0)(1) <= rngs(1)(1)
                            val lowerInsideUpper = rngs(1)(0) >= rngs(0)(0) && rngs(1)(1) <= rngs(0)(1)
                            upperInsideLower || lowerInsideUpper
                          else
                            val startOverlaps = (rngs(0)(0) >= rngs(1)(0) && rngs(0)(0) <= rngs(1)(1)) || (rngs(1)(0) >= rngs(0)(0) && rngs(1)(0) <= rngs(0)(1))
                            val endOverlaps = (rngs(0)(1) <= rngs(1)(1) && rngs(0)(1) >= rngs(1)(0)) || (rngs(1)(1) <= rngs(0)(1) && rngs(1)(1) >= rngs(0)(0))
                            startOverlaps || endOverlaps)
          //If they overlap, increase counter
          if overlaps then
            overlapCount += 1
          line = lineReader.readLine()
        overlapCount
      finally
        fileReader.close()
        lineReader.close()
    catch
      case notFound: FileNotFoundException => println("File wasn't connected correctly")
      case _ => println("Problems with reading the file")
  end findOverlap

  println(s"Found ${findOverlap(true)} completely overlapping ranges")
  println(s"Found ${findOverlap(false)} partly overlapping ranges")

end rangeCounter

