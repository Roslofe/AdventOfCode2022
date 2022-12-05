package day5
import java.io._

/** The purpose of crateStacker is to 'move' different crates, and then report which crates are at the top of each stack.
 * For more information, see: https://adventofcode.com/2022/day/5 */
object crateStacker extends App:

  /** moveCrates initialises, moves, and displays the results of the move.
   * @param first   Determines which half of the exercise is being executed.
   * @return        The top crates of each stack. */
  private def moveCrates(first: Boolean) =
    try
      val file = File("./src/main/scala/day5/data.txt")
      val fileReader = FileReader(file)
      val lineReader = BufferedReader(fileReader)
      //Attempt to read file data
      try
        var line = lineReader.readLine()
        //Determine number of stacks, and create them
        val crateAmount = (line.length.toDouble / 4).ceil.toInt
        val crateStacks = Array.fill(crateAmount)(scala.collection.mutable.ListBuffer[Char]())
        //First, initialise the crates' current positions
        while line != "" do
          if !line.trim()(0).isDigit then
            val crates = line.grouped(4).zipWithIndex.toArray
            for crate <- crates do
              val trimmed = crate._1.trim
              if trimmed != "" then
                crateStacks(crate._2).append(trimmed(1))
          line = lineReader.readLine()
        line = lineReader.readLine()
        //For examining the first section of the exercise
        if first then
          //Add the order of moving the crates
          //From, to
          val moveOrder = scala.collection.mutable.Queue[(Int, Int)]()
          while line != null do
            val instructions = line.split(" ")
            var i = 0
            while i < instructions(1).toInt do
              moveOrder.append((instructions(3).toInt - 1, instructions(5).toInt - 1))
              i += 1
            line = lineReader.readLine()
          //Finally, move the crates
          while moveOrder.nonEmpty do
            val currentMove = moveOrder.dequeue()
            val fromStack = crateStacks(currentMove._1)
            val moving = fromStack(0)
            fromStack.remove(0)
            crateStacks(currentMove._2).prepend(moving)
        //Second half of the exercise, moving multiple stacks at a time
        else
          while line != null do
            val instructions = line.split(" ")
            val movingStack = crateStacks(instructions(3).toInt - 1)
            val moving = movingStack.take(instructions(1).toInt)
            movingStack.remove(0, instructions(1).toInt)
            crateStacks(instructions(5).toInt - 1).prependAll(moving)
            line = lineReader.readLine()
        crateStacks.map(_(0)).mkString
      finally
        fileReader.close()
        lineReader.close()
    catch
      case notFound: FileNotFoundException => println("File wasn't connected correctly")
      case _ => println("Problems with reading the file")
  end moveCrates

  println(s"Final top crates: ${moveCrates(true)}")
  println(s"Final top crates, when moving multiple simultaneously: ${moveCrates(false)}")

end crateStacker

