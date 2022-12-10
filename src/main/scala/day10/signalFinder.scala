package day10
import java.io._
import scala.collection.mutable.ListBuffer

/** signalFinder goes through a set of actions provided in the data, each taking a specified amount of time, and affecting the value of the variable x.
 * For more information, see: https://adventofcode.com/2022/day/10 */
object signalFinder extends App:

  /** findStrenghts initialises all the instructions given in the data. After that, it traverses through the actions,
   * updating the x-value when needed. When the clock is at specified values (seen in clockValues), the function
   * calculates the signal strenght of the current x, and adds it to the list.
   * @return    The sum of the signal values at the desired clock values
   */
  private def findStrengths() =
    try
      val file = File("./src/main/scala/day10/data.txt")
      val fileReader = FileReader(file)
      val lineReader = BufferedReader(fileReader)
      //Attempt to read file data
      try
        //Keep track of the current cycle
        var cycle = 1
        //Store the desired values of x
        val xValues = ListBuffer[Int]()
        //The cycles when the value of x is being evaluated
        val clockValues = Array(20, 60, 100, 140, 180, 220)
        //Latest value of x
        var currX = 1
        //Store the actions being done, and the number of cycles they require to be completed
        val waitingActions = ListBuffer[(Int, Int)]()
        var line = lineReader.readLine()
        //Add all the actions into the list of actions
        while line != null do
          val splitInfo = line.split(" ")
          /** Add the action provided in the data to the list of them.
           * Noop takes one cycle to complete, and addx takes two
           */
          waitingActions.addOne(if splitInfo(0) == "noop" then (0, 1) else (splitInfo(1).toInt, 2))
          line = lineReader.readLine()
        //Go through each action, and add the applicable ones to the List
        while waitingActions.nonEmpty do
          //Check if there is an action that is done
          val currAction = waitingActions(0)
          if currAction._2 == 0 then
            waitingActions.remove(0)
            currX += currAction._1
          //Is the current cycle of interest? If so, add the signal strenght to the List
          if clockValues.contains(cycle) then
            xValues.addOne(currX * cycle)
          //Cycle is complete, increase clock value
          cycle += 1
          //Decrease the cycles needed to complete the next action
          //A new variable is needed, since currAction might've been completed
          if waitingActions.nonEmpty then
            val nextAction = waitingActions(0)
            waitingActions.update(0, (nextAction._1, nextAction._2 - 1))

        //Return the sum of the signal strenghts
        xValues.toArray.sum
      finally
        fileReader.close()
        lineReader.close()
      catch
        case notFound: FileNotFoundException => println("File wasn't connected correctly")
        case _ => println("Problems with reading the file")
  end findStrengths

  println(s"Sum of strenghts: ${findStrengths()}")

end signalFinder

