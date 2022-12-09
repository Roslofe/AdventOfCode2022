package day9
import java.io._
import scala.collection.mutable.Queue
import scala.collection.mutable.ListBuffer
import scala.math.abs

/** The purpose of the object is to move the rope head according to the insructions provided by the data.
 * The tail of the rope is moved based on the head's movement, and the locations visited by the tail are tracked.
 * For more information, see:https://adventofcode.com/2022/day/9 */
object positionTracer extends App:

  /** Moves the rope head and tail according to the data.
   * @return  The number of locations visited by the tail
   */
  private def traceSteps() =
    try
      val file = File("./src/main/scala/day9/data.txt")
      val fileReader = FileReader(file)
      val lineReader = BufferedReader(fileReader)
      //Attempt to read file data
      try
        //Insert all the steps into a Queue
        val stepOrder = Queue[String]()
        var line = lineReader.readLine()
        while line != null do
          val split = line.split(" ")
          //Add each of the head's moves to the Queue, as many times as it's done
          for i <- 0 until split(1).toInt do
            stepOrder.enqueue(split(0))
          line = lineReader.readLine()
        //Stores the coordinates of each position that has been visited
        //The starting position is assumed to be 0,0
        val visited = ListBuffer((0, 0))

        /** Determine if the tail should move, a.k.a if it's not touching the head
         * @param tail  Current position of the tail
         * @param head  Current position of the head
         * @return      Boolean value signifying whether the tail nees to move
         */
        def shouldMove(tail: (Int, Int), head: (Int, Int)) =
          val horDist = abs(tail._1 - head._1)
          val vertDist = abs(tail._2 - head._2)
          if horDist <= 1 && vertDist <= 1 then
            false
          else
            true
        end shouldMove

        /** Determines the new position for the head based on the instructions.
         * @param action  The command given by the data
         * @param x       Current x-coordinate of the head
         * @param y       Current y-coordinate of the head
         * @return        The new position of the head
         */
        def updateHPos(action: String, x: Int, y: Int) =
          action match
            case "U" => (x, y + 1)
            case "D" => (x, y - 1)
            case "L" => (x - 1, y)
            case "R" => (x + 1, y)
            case _ => (0, 0)
        end updateHPos

        /** Determines what the new position of the tail is. The tail must always be within one measure of the head.
         * @param tail    The coordinates of the tail
         * @param head    The coordinates of the head
         * @return        The new position of the tail
         */
        def moveTail(tail: (Int, Int), head: (Int, Int)) =
          //If the head and tail are parallel, update tail to be next to the head
          if tail._1 == head._1 then
            (tail._1, if head._2 - tail._2 > 0 then tail._2 + 1 else tail._2 - 1)
          else if tail._2 == head._2 then
            (if head._1 - tail._1 > 0 then tail._1 + 1 else tail._1 - 1, tail._2)
          else if head._1 - tail._1 > 0 && head._2 - tail._2 > 0 then
            (tail._1 + 1, tail._2 + 1)
          else if head._1 - tail._1 < 0 && head._2 - tail._2 < 0 then
            (tail._1 - 1, tail._2 - 1)
          else if head._1 - tail._1 > 0 then
            (tail._1 + 1, tail._2 - 1)
          else
            (tail._1 - 1, tail._2 + 1)
        end moveTail

        //Positions of the head and the tail (x, y)
        var headPos = (0, 0)
        var tailPos = (0, 0)
        //Go through each step, and update the positions of the head and tail accordingly
        while stepOrder.nonEmpty do
          val current = stepOrder.dequeue
          headPos = updateHPos(current, headPos._1, headPos._2)
          //If the distance between the head and the tail is more than 1, move the tail
          if shouldMove(tailPos, headPos) then
            tailPos = moveTail(tailPos, headPos)
            visited.append(tailPos)

        //Return the number of distinct positions the tail got
        visited.toArray.distinct.length

      finally
        fileReader.close()
        lineReader.close()
    catch
      case notFound: FileNotFoundException => println("File wasn't connected correctly")
      case _ => println("Problems with reading the file")
  end traceSteps

  println(s"Visited ${traceSteps()} different locations")

end positionTracer

