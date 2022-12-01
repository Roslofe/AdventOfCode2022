package day1
import java.io._
import scala.math.max

/**The purpose of the object is to find the elf carrying the largest amount of calories, and return said calorie amount. For more information, see: https://adventofcode.com/2022/day/1*/
object calorieCounter extends App:
  //FindCals can find either the maximum individual calories, or the sum of the top three, as requested in the second half. This is determined by the top3 variable
  private def findCals(top3: Boolean) =
    //Initialising variables to read the file
    try
      val file = File("./src/main/scala/day1/data.txt")
      val fileReader = FileReader(file)
      val lineReader = BufferedReader(fileReader)
      //Attempt to read file data
      try
        //Store the max number of calories, or array of top 3. The one being used depends on the top3 variable
        var currentMax = 0
        var top = Array.fill(3)(0)
        var line = lineReader.readLine()
        var currentCals = 0
        while line != null do
          //If all the items from the elf have been processed, update the max if needed, and set the counter to 0
          if line == "" then
            if top3 then
              //Check if amount is larger than one of the ones in the array. If so, update the appropriate index
              val maxComparison = top.map(x => x.compare(currentCals))
              val replacingIndex = maxComparison.indexWhere(_ == -1)
              //Change the rest of the top if needed
              var i = 2
              while i > replacingIndex && replacingIndex != -1 do
                top.update(i, top(i - 1))
                i -= 1
              if replacingIndex != -1 then
                top.update(replacingIndex, currentCals)
            else
              currentMax = max(currentMax, currentCals)
            currentCals = 0
          //Otherwise add the read amount to the counter
          else
            currentCals += line.toInt
          line = lineReader.readLine()
        //Return the max amount of calories
        if top3 then
          top.sum
        else
        currentMax
      finally
        fileReader.close()
        lineReader.close()
    catch
      case notFound: FileNotFoundException => println("File wasn't found correctly")
      case _ => println("Problems with reading the file")
  end findCals

  println(s"Found individual maximum: ${findCals(false)}")
  println(s"Found group maximum: ${findCals(true)}")

end calorieCounter

