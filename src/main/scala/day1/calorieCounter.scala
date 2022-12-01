package day1
import java.io._
import scala.math.max

/**The purpose of the object is to find the elf carrying the largest amount of calories, and return said calorie amount. For more information, see: https://adventofcode.com/2022/day/1*/
object calorieCounter extends App:
  private def findCals() =
    //Initialising variables to read the file
    try
      val file = File("./src/main/scala/day1/data.txt")
      val fileReader = FileReader(file)
      val lineReader = BufferedReader(fileReader)
      //Attempt to read file data
      try
        //Store the max number of calories
        var currentMax = 0
        var line = lineReader.readLine()
        var currentCals = 0
        while line != null do
          //If all the items from the elf have been processed, update the max if needed, and set the counter to 0
          if line == "" then
            currentMax = max(currentMax, currentCals)
            currentCals = 0
          //Otherwise add the read amount to the counter
          else
            currentCals += line.toInt
          line = lineReader.readLine()
        //Return the max amount of calories
        currentMax
      finally
        fileReader.close()
        lineReader.close()
    catch
      case notFound: FileNotFoundException => println("File wasn't found correctly")
      case _ => println("Problems with reading the file")
  end findCals

  println(s"Found maximum: ${findCals()}")

end calorieCounter

