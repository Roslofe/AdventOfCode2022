package day12
import java.io._
import scala.collection.mutable.ListBuffer
import scala.math.abs

/** mountainNavigator moves around a mountain, attempting to find the shortest path from the start location to the end location.
 * Only small differences of altitude can happen at each step. For more information, see: https://adventofcode.com/2022/day/12 */
object mountainNavigator extends App:

  /** Forms a graph from the given data, and then traverses it to find the shortest path to the given target-
   * In the second half,no explisit start point is given, instead all locations with the lowest altitude are considered start points
   * @param first   Determines which half of the exercise is being evaluated
   * @return        The number of steps needed to travel from the start node to the end node */
  private def findRoute(first: Boolean) =
    try
      val file = File("./src/main/scala/day12/data.txt")
      val fileReader = FileReader(file)
      val lineReader = BufferedReader(fileReader)
      //Attempt to read file data
      try
        var line = lineReader.readLine()
        //Store the data of the different locations
        //1st number represents the altitude, and the second the status (1 = start, 0 = normal, -1 = end)
        val elevationData = ListBuffer[Array[(Int, Int)]]()
        while line != null do
          //Change the text input into location objects, add to the list of elevations
          elevationData += line.toCharArray.map(x=> if x == 'S' then ('a'.toInt, 1) else if x == 'E' then ('z'.toInt, -1) else (x.toInt, 0))
          line = lineReader.readLine()

        //Store the distances of each point from the start point. By default it's -1, when it hasn't been visited. 2nd parameter is the parent node, and 3rd is whether it has been selected, 4th = elevation
        val locations = elevationData.flatten.map(x => (-1, -1, false, x._1)).toArray
        //Array.fill(elevationData.length * elevationData(0).length)((-1, -1, false, -1))

        /** formEdges processes the elevation data provided, and transforms it into edges that connect locations.
         * Practically forms a simple graph.
         * @return The edges between nodes, the indices of the start and end nodes.
         */
        def formEdges() =
          var start = -1
          var end = -1
          val rowSize = elevationData(0).length
          val edges = ListBuffer[(Int, Int)]()
          var i = 0
          while i < elevationData.length do
            var v = 0
            while v < elevationData(0).length do
              val current = elevationData(i)(v)
              val currentVal = (i * rowSize) + v
              if current._2 == 1 then
                start = currentVal
              else if current._2 == -1 then
                end = currentVal
              //Find the neighbouring nodes, and add the connecting edges to the list
              //Neighbouring nodes share an edge if their altitude differs by at most one
              if i != 0 then
                val topNeigh = (((i - 1) * rowSize) + v, elevationData(i - 1)(v)._1)
                if abs(topNeigh._2 - current._1) <= 1 then
                  edges += ((currentVal, topNeigh._1))
              if i != elevationData.length - 1 then
                val bottomNeigh = (((i + 1) * rowSize) + v, elevationData(i + 1)(v)._1)
                if abs(bottomNeigh._2 - current._1) <= 1 then
                  edges += ((currentVal, bottomNeigh._1))
              if v != 0 then
                val leftNeigh = ((i * rowSize) + (v - 1), elevationData(i)(v - 1)._1)
                if abs(leftNeigh._2 - current._1) <= 1 then
                  edges += ((currentVal, leftNeigh._1))
              if v != rowSize - 1 then
                val rightNeigh = ((i * rowSize) + (v + 1), elevationData(i)(v + 1)._1)
                if abs(rightNeigh._2 - current._1) <= 1 then
                  edges += ((currentVal, rightNeigh._1))
              v += 1
            i += 1
          (edges.toArray, start, end)
        end formEdges

        //Store the information about the edges, as well as the start and end
        val edgeFormat = formEdges()
        val edges = edgeFormat._1
        val start = edgeFormat._2
        val end = edgeFormat._3

        //If in the first half, update the distance of the start point to 0
        if first then
          locations.update(start, (0, -1, false, locations(start)._4))
        else
          //In the second half, set the distance of the end point to 0
          locations.update(end, (0, -1, false, locations(end)._4))
        var inaccessible = false
        //Modified version of Djikstra's algorithm (this doesn't use weights, as all edges have the same weight)
        //Finds the shortest distance to the end point
        while !locations.forall(_._3) && !inaccessible do
          //Take the edge with the shortest distance, that has been visited
          val unvisited = locations.filter(x => !x._3 && x._1 != -1)
          if unvisited.isEmpty then
            inaccessible = true
          else
            val closest = locations.indexOf(unvisited.minBy(_._1))
            val closestInfo = locations(closest)
            //Mark the chosen edge as done
            locations.update(closest, (closestInfo._1, closestInfo._2, true, closestInfo._4))
            //Get the neighbour nodes that haven't been selected yet
            val neighs = edges.filter(x => x._1 == closest).map(x => (x._2, locations(x._2))).filter(x => !x._2._3)
            //Go through each neighbour. If they don't have a parent node yet, add them with the current as the parent
            //If they have a parent, but the distance is less with the current one, change the parent
            var i = 0
            while i < neighs.length do
              val currNeigh = neighs(i)
              if currNeigh._2._2 == -1 || currNeigh._2._2 + 1 > closestInfo._2 + 1 then
                locations.update(currNeigh._1, (closestInfo._1 + 1, closest, false, currNeigh._2._4))
              i += 1

        if first then
          //Return the distance to the end point
          locations(end)._1
        else
          //Find the minimum distance to a point with the altitude of 97
          locations.filter(x  => x._4 == 'a'.toInt && x._3).minBy(_._1)._1

      finally
        fileReader.close()
        lineReader.close()
    catch
      case notFound: FileNotFoundException => println("File wasn't connected correctly")
      case _ => println("Problems with reading the file")
  end findRoute

  println(s"Shortest distance to the end point: ${findRoute(true)}")
  println(s"Shortest distance from smallest altitude: ${findRoute(false)}")
  
end mountainNavigator

